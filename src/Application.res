@val external window: {..} = "window"
@send external click: Dom.element => unit = "click"

module Tree = {
  type rec tree<'n> = {
    label: 'n,
    forest: array<tree<'n>>,
  }
  type selection = {
    range: array<int>,
    hasChildren: bool,
  }
  let select = (ts, selection) => {
    let (_, s, fs) = Belt.Array.reduce(selection, (true, [Array.length(ts)], ts), (acm, p) => {
      switch acm {
      | (true, ss, ts) =>
        switch Belt.Array.get(ts, p) {
        | Some(n) => (true, Belt.Array.concat(ss, [Array.length(n.forest)]), n.forest)
        | _ => acm
        }
      | _ => acm
      }
    })
    {range: s, hasChildren: Array.length(fs) > 0}
  }
}

let {select: selectTree, select} = module(Tree)
type tree<'n> = Tree.tree<'n>

type valueUnit = {
  id: int,
  name: string,
}
type schemaBody =
  | SNumber({label: string, unit: valueUnit})
  | SText(string)
type valueBody =
  | VNumber(int)
  | VText(string)

type schema = {
  id: int,
  name: string,
  body: tree<schemaBody>,
}
type value = tree<valueBody>

type partion<'v> = {
  schema: schema,
  value: 'v,
}
type schemaSelection = {cursor: array<int>, mark: array<int>}
type selection =
  | SchemaSelection(schemaSelection)
  | ValueSelection({cursor: array<int>, mark: array<int>})

type masterSlaveValue = {master: value, slaves: array<value>}

type container =
  | MasterSlave(partion<masterSlaveValue>)
  | Transitional(partion<array<value>>)
  | Enumerable(partion<array<value>>) // default

let containerSchema = (container: container) => {
  switch container {
  | Enumerable({schema}) => schema
  | MasterSlave({schema}) => schema
  | Transitional({schema}) => schema
  }
}

type keySet = {
  altKey: bool,
  shiftKey: bool,
  ctrlKey: bool,
  code: string,
}
let eq = (a: keySet, b: keySet) => {
  a.code === b.code && a.shiftKey === b.shiftKey && a.ctrlKey === b.ctrlKey && a.altKey === b.altKey
}
let keySet = (~shiftKey=false, ~ctrlKey=false, ~altKey=false, code: string): keySet => {
  {
    code: code,
    shiftKey: shiftKey,
    ctrlKey: ctrlKey,
    altKey: altKey,
  }
}

type keySetting = {
  keyUp: array<keySet>,
  keyDown: array<keySet>,
  keyLeft: array<keySet>,
  keyRight: array<keySet>,
  keyEnter: array<keySet>,
}

type setting = {keySetting: keySetting}

type state = {
  selection: selection,
  container: container,
  setting: setting,
}

type direction =
  | Left
  | Up
  | Right
  | Down

type action =
  | Add(int)
  | CursorAction(direction)
  | MarkAction
  | SwapAction
  | Load(state)

let moveCursorAction = (state, selectAction) => {
  let {selection, container} = state
  let next = switch selection {
  | SchemaSelection({cursor, mark}) => {
      let selectDepth = Belt.Array.length(cursor)
      let {body} = containerSchema(container)
      let selection = select([body], cursor)
      let {range, hasChildren} = selection
      let rangeDepth = Belt.Array.length(range)

      let cursor = switch selectAction {
      | Left => selectDepth > 1 ? Belt.Array.slice(cursor, ~len=selectDepth - 1, ~offset=0) : cursor
      | Right =>
        hasChildren
          ? Belt.Array.concat(Belt.Array.slice(cursor, ~len=rangeDepth, ~offset=0), [0])
          : cursor
      | Down =>
        if selectDepth <= rangeDepth {
          let currentIndex = cursor[selectDepth - 1]
          let currentForestMaxLength = range[selectDepth - 1]
          currentIndex < currentForestMaxLength - 1
            ? Belt.Array.concat(
                Belt.Array.slice(cursor, ~len=selectDepth - 1, ~offset=0),
                [currentIndex + 1],
              )
            : cursor
        } else {
          cursor
        }

      | Up =>
        if selectDepth <= rangeDepth {
          let currentIndex = cursor[selectDepth - 1]

          0 < currentIndex
            ? Belt.Array.concat(
                Belt.Array.slice(cursor, ~len=selectDepth - 1, ~offset=0),
                [currentIndex - 1],
              )
            : cursor
        } else {
          cursor
        }
      }

      {
        ...state,
        selection: SchemaSelection({
          mark: mark,
          cursor: cursor,
        }),
      }
    }
  | ValueSelection(_) => state
  }
  Js.log(next)
  next
}

let swapUpdate = state => {
  let {selection, container} = state

  switch selection {
  | SchemaSelection({cursor, mark}) if Array.length(mark) > 0 && Array.length(cursor) > 0 => {
      let {id, name, body} = containerSchema(container)
      let markLen = Array.length(mark)
      let cursorLen = Array.length(cursor)
      if markLen === cursorLen {
        // todo: fix this algorithm
        let rec r = (ts: array<tree<schemaBody>>, pos: array<int>): array<tree<schemaBody>> => {
          Belt.Array.mapWithIndex(ts, (i, t) => {
            let p = Belt.Array.concat(pos, [i])
            if markLen === Array.length(p) {
              if Belt.Array.eq(p, mark, (a, b) => a === b) {
                ts[cursor[markLen - 1]]
              } else if Belt.Array.eq(p, cursor, (a, b) => a === b) {
                ts[mark[markLen - 1]]
              } else {
                t
              }
            } else {
              {
                label: t.label,
                forest: r(t.forest, p),
              }
            }
          })
        }
        {
          ...state,
          container: Enumerable({
            schema: {
              id: id,
              name: name,
              body: {
                label: body.label,
                forest: r(body.forest, [0]),
              },
            },
            value: [],
          }),
          selection: SchemaSelection({
            cursor: cursor,
            mark: [],
          }),
        }
      } else {
        // todo: implment
        state
      }
    }
  | _ => state
  }
}

let isFilled = (a: array<int>, b: array<int>): bool => {
  Belt.Array.length(a) <= Belt.Array.length(b) &&
    Belt.Array.reduceWithIndex(a, true, (acm, p, i) => {
      acm && p === Belt.Array.getUnsafe(b, i)
    })
}

type mapTree<'n> = (tree<'n>, (array<int>, tree<'n>) => array<tree<'n>>) => tree<'n>
let mapTree: mapTree<'n> = (t, fn): tree<'n> => {
  let rec traverse = (fs: array<tree<'n>>, pos: array<int>) => {
    Belt.Array.concatMany(
      Belt.Array.mapWithIndex(fs, (i, t): array<tree<'n>> => {
        let p = Belt.Array.concat(pos, [i])
        Belt.Array.map(fn(p, t), (tt): tree<'n> => {
          {
            label: tt.label,
            forest: traverse(tt.forest, p),
          }
        })
      }),
    )
  }
  {
    label: t.label,
    forest: traverse(t.forest, [0]),
  }
}

let reducer = (state, action) => {
  switch action {
  | Add(_) => state
  | CursorAction(v) => moveCursorAction(state, v)
  | MarkAction =>
    switch state.selection {
    | SchemaSelection({cursor, mark}) => {
        let markedUpdated = {
          ...state,
          selection: SchemaSelection({cursor: cursor, mark: cursor}),
        }
        Array.length(mark) > 0 && Array.length(cursor) > 0 ? swapUpdate(state) : markedUpdated
      }
    | ValueSelection({cursor}) => {
        ...state,
        selection: ValueSelection({cursor: cursor, mark: cursor}),
      }
    }
  | SwapAction => swapUpdate(state)
  | Load(state) => state
  }
}

module SchemaLabel = {
  @react.component
  let make = (~label: schemaBody, ~position: array<int>, ~selection: option<schemaSelection>) => {
    Js.log(selection)
    Js.log(position)
    let (isSelected, isMarked) = switch selection {
    | Some({cursor, mark}) => (
        Belt.Array.eq(position, cursor, (a, b) => a === b),
        Belt.Array.eq(position, mark, (a, b) => a === b),
      )
    | _ => (false, false)
    }
    <div
      style={ReactDOM.Style.make(
        ~backgroundColor=isSelected
          ? "rgba(128, 128, 96, 0.5)"
          : isMarked
          ? "rgba(216, 255, 216, 1)"
          : "transparent",
        ~paddingLeft=`${(Belt.Array.length(position) * 24)->Js.Int.toString}px`,
        (),
      )}>
      {switch label {
      | SNumber({label, unit: {id: _id, name: _name}}) =>
        <div> {React.string(label)} <span> {React.string("(number)")} </span> </div>
      | SText(label) => <div> {React.string(label)} <span> {React.string("(label)")} </span> </div>
      }}
    </div>
  }
}
type schemaBlockProps = {
  tree: tree<schemaBody>,
  selection: option<schemaSelection>,
  position: array<int>,
}

module type SchemaBlock = {
  type props = schemaBlockProps
  @obj
  external makeProps: (
    ~tree: tree<schemaBody>,
    ~selection: option<schemaSelection>=?,
    ~position: array<int>,
    ~key: string,
    unit,
  ) => props = ""
  let make: schemaBlockProps => React.element
}

module rec SchemaBlock: SchemaBlock = {
  type props = schemaBlockProps

  @obj
  external makeProps: (
    ~tree: tree<schemaBody>,
    ~selection: option<schemaSelection>=?,
    ~position: array<int>,
    ~key: string,
    unit,
  ) => props = ""

  let make = ({tree, selection, position}) => {
    <>
      <SchemaLabel label={tree.label} position={position} selection={selection} />
      {Belt.Array.mapWithIndex(tree.forest, (i, f) => {
        let pos = Belt.Array.concat(position, [i])
        let key = Belt.Array.reduce(pos, "", (acm, p) => `${acm} ${p->Belt.Int.toString}`)
        <SchemaBlock tree={f} selection={selection} position={pos} key={key} />
      })->React.array}
    </>
  }
}

module ValueLabel = {
  @react.component
  let make = (~value: valueBody) => {
    <div>
      {switch value {
      | VNumber(n) => n->Belt.Int.toString
      | VText(t) => t
      }->React.string}
    </div>
  }
}

module ValueBlock = {
  @react.component
  let make = (~value: value) => {
    <ValueLabel value={value.label} />
  }
}
module ValueArray = {
  @react.component
  let make = (~value: array<value>) => {
    <div style={ReactDOM.Style.make(~display="flex", ~flexDirection="column", ~padding="48px", ())}>
      {Belt.Array.mapWithIndex(value, (i, v) =>
        <ValueBlock value={v} key={Belt.Int.toString(i)} />
      )->React.array}
    </div>
  }
}

let mock = {
  selection: SchemaSelection({cursor: [0], mark: []}),
  container: Enumerable({
    schema: {
      id: 1,
      name: "schema",
      body: {
        label: SText(`schema`),
        forest: [],
      },
    },
    value: [
      {
        label: VText(`value`),
        forest: [],
      },
    ],
  }),
  setting: {
    keySetting: {
      keyDown: [keySet("ArrowDown"), keySet("KeyJ")],
      keyLeft: [keySet("ArrowLeft"), keySet("KeyH")],
      keyRight: [keySet("ArrowRight"), keySet("KeyL")],
      keyEnter: [keySet("KeyM"), keySet("Enter")],
      keyUp: [keySet("ArrowUp"), keySet("KeyK")],
    },
  },
}

module DownloadButton = {
  @react.component
  let make = (~content: state) => {
    let anchor = React.useRef(Js.Nullable.null)
    <>
      <button
        onClick={_ => {
          anchor.current->Js.Nullable.toOption->Belt.Option.forEach(a => a->click)
        }}>
        {"download"->React.string}
      </button>
      <a
        ref={ReactDOM.Ref.domRef(anchor)}
        href={`data:,${Belt.Option.getWithDefault(Js.Json.stringifyAny(content), "")}`}
        download="dim.json"
        style={ReactDOM.Style.make(~display="none", ())}>
        {React.string("anchor")}
      </a>
    </>
  }
}
module UploadButton = {
  type props = {upload: state => unit}
  @react.component
  let make = (~_upload_: state => unit) => {
    let inputRef = React.useRef(Js.Nullable.null)
    <>
      <input type_={"file"} accept={".json, .text, .txt"} ref={ReactDOM.Ref.domRef(inputRef)} />
      <button
        onClick={%raw(`() => {
            if(inputRef.current && inputRef.current.files.length > 0) {
                const f = inputRef.current.files[0]
                const reader = new FileReader()
                reader.onload = evt => {
                    const r = JSON.parse(evt.target.result)
                    Props._upload_(r)
                }
                reader.readAsText(f, "UTF-8")
            }
        }`)}>
        {React.string("upload")}
      </button>
    </>
  }
}

module App = {
  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, mock)
    let {selection, container, setting: {keySetting}} = state
    let Enumerable({schema: {body}, value}) = container

    let {keyDown, keyUp, keyLeft, keyRight, keyEnter} = keySetting

    React.useEffect1(() => {
      let onInput = (inp: keySet, s: array<keySet>, fn: unit => unit) => {
        let r = Belt.Array.some(s, k => eq(k, inp))
        if r {
          fn()
        }
        r
      }
      let handleInput = evt => {
        let input: keySet = {
          code: evt["code"],
          shiftKey: evt["shiftKey"],
          altKey: evt["altKey"],
          ctrlKey: evt["ctrlKey"],
        }
        let handle = input->onInput

        Belt.Array.some(
          [
            keyDown->handle(() => {
              dispatch(CursorAction(Down))
            }),
            keyUp->handle(() => {
              dispatch(CursorAction(Up))
            }),
            keyLeft->handle(() => {
              dispatch(CursorAction(Left))
            }),
            keyRight->handle(() => {
              dispatch(CursorAction(Right))
            }),
            keyEnter->handle(() => {
              dispatch(MarkAction)
            }),
          ],
          c => c,
        )
      }
      ignore(window["addEventListener"](. "keydown", handleInput))
      Some(() => window["removeEventListener"](. "keydown", handleInput))
    }, [keySetting])

    let schemaSelection = switch selection {
    | SchemaSelection(v) => Some(v)
    | _ => None
    }
    <div>
      <SchemaBlock tree={body} selection={schemaSelection} position={[0]} key="root" />
      <ValueArray value={value} />
      <div
        style={ReactDOM.Style.make(
          ~position="fixed",
          ~bottom="0",
          ~right="0",
          ~display="flex",
          ~flexDirection="column",
          (),
        )}>
        <DownloadButton content={state} />
        <UploadButton _upload_={state => dispatch(Load(state))} />
      </div>
    </div>
  }
}

@react.component
let make = () => {
  <App />
}
