@val external window: {..} = "window"
@send external click: Dom.element => unit = "click"
@send external focus: Dom.element => unit = "focus"

module Tree = {
  open Belt
  type rec tree<'n> = {
    label: 'n,
    forest: array<tree<'n>>,
  }
  type selection = {
    range: array<int>,
    hasChildren: bool,
  }
  let select = (ts, selection) => {
    let (_, s, fs) = Array.reduce(selection, (true, [Array.length(ts)], ts), (acm, p) => {
      switch acm {
      | (true, ss, ts) =>
        switch ts[p] {
        | Some(n) => (true, Array.concat(ss, [Array.length(n.forest)]), n.forest)
        | _ => acm
        }
      | _ => acm
      }
    })
    {range: s, hasChildren: Array.length(fs) > 0}
  }
  let map = (t: tree<'n>, fn: (array<int>, tree<'n>) => array<tree<'n>>): array<tree<'n>> => {
    let rec traverse = (fs: array<tree<'n>>, pos: array<int>) => {
      fs
      ->Array.mapWithIndex((i, t): array<tree<'n>> => {
        let p = Array.concat(pos, [i])
        fn(p, t)->Array.map((tt): tree<'n> => {
          {
            label: tt.label,
            forest: traverse(tt.forest, p),
          }
        })
      })
      ->Array.concatMany
    }
    traverse([t], [])
  }
  let get = (t: tree<'n>, index: array<int>): option<'n> => {
    index
    ->Array.reduce(Some(t), (acm, i) => {
      acm->Option.flatMap(t => t.forest[i])
    })
    ->Option.map(t => t.label)
  }
}

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
  keyAppend: array<keySet>,
  keyScrollUp: array<keySet>,
  keyScrollDown: array<keySet>,
  keyEdit: array<keySet>,
  escape: array<keySet>,
}

type setting = {keySetting: keySetting}

let getKeyboardSetting = (~ks: option<keySetting>=?, ()): keySetting => {
  let mapping = (
    ~keyUp: array<keySet>=[keySet("ArrowUp"), keySet("KeyK")],
    ~keyDown: array<keySet>=[keySet("ArrowDown"), keySet("KeyJ")],
    ~keyLeft: array<keySet>=[keySet("ArrowLeft"), keySet("KeyH")],
    ~keyRight: array<keySet>=[keySet("ArrowRight"), keySet("KeyL")],
    ~keyEnter: array<keySet>=[keySet("KeyM"), keySet("Enter")],
    ~keyAppend: array<keySet>=[keySet("KeyO")],
    ~keyScrollDown: array<keySet>=[keySet("KeyF", ~ctrlKey=true)],
    ~keyScrollUp: array<keySet>=[keySet("KeyB", ~ctrlKey=true)],
    ~keyEdit: array<keySet>=[keySet("KeyE")],
    ~escape: array<keySet>=[keySet("Escape")],
    (),
  ): keySetting => {
    keyUp: keyUp,
    keyDown: keyDown,
    keyLeft: keyLeft,
    keyRight: keyRight,
    keyEnter: keyEnter,
    keyAppend: keyAppend,
    keyScrollDown: keyScrollDown,
    keyScrollUp: keyScrollUp,
    keyEdit: keyEdit,
    escape: escape,
  }
  switch ks {
  | Some({keyDown, keyUp, keyLeft, keyRight, keyEnter, keyAppend, keyEdit, escape}) =>
    mapping(~keyDown, ~keyUp, ~keyLeft, ~keyRight, ~keyEnter, ~keyAppend, ~keyEdit, ~escape, ())
  | _ => mapping()
  }
}

type mode =
  | Normal
  | Edit

type state = {
  selection: selection,
  container: container,
  setting: setting,
  mode: mode,
}

type direction =
  | Left
  | Up
  | Right
  | Down

type action =
  | Add(int)
  | Append
  | Edit(array<int>, schemaBody)
  | CursorAction(direction)
  | MarkAction
  | SwapAction
  | Load(state)
  | ChangeMode(mode)

let moveCursorAction = (state, selectAction) => {
  open Belt
  let {selection, container} = state
  let next = switch selection {
  | SchemaSelection({cursor, mark}) => {
      let selectDepth = Array.length(cursor)
      let {body} = containerSchema(container)
      let selection = Tree.select([body], cursor)
      let {range, hasChildren} = selection
      let rangeDepth = Array.length(range)

      let cursor = switch selectAction {
      | Left if selectDepth > 1 => Array.slice(cursor, ~len=selectDepth - 1, ~offset=0)
      | Right if hasChildren => Array.concat(Array.slice(cursor, ~len=rangeDepth, ~offset=0), [0])
      | Down if selectDepth <= rangeDepth =>
        switch (cursor[selectDepth - 1], range[selectDepth - 1]) {
        | (Some(currentIndex), Some(currentForestMaxLength))
          if currentIndex < currentForestMaxLength - 1 =>
          Array.concat(Array.slice(cursor, ~len=selectDepth - 1, ~offset=0), [currentIndex + 1])
        | _ => cursor
        }
      | Up if selectDepth <= rangeDepth =>
        cursor[selectDepth - 1]->Option.mapWithDefault(cursor, currentIndex =>
          0 < currentIndex
            ? Array.concat(Array.slice(cursor, ~len=selectDepth - 1, ~offset=0), [currentIndex - 1])
            : cursor
        )
      | _ => cursor
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
  next
}

let swapUpdate = state => {
  open Belt
  let {selection, container} = state

  switch selection {
  | SchemaSelection({cursor, mark}) if Array.length(mark) > 0 && Array.length(cursor) > 0 => {
      let {id, name, body} = containerSchema(container)
      let markLen = Array.length(mark)
      let cursorLen = Array.length(cursor)
      if markLen == cursorLen {
        // todo: fix this algorithm
        let rec traverse = (ts: array<tree<schemaBody>>, pos: array<int>): array<
          tree<schemaBody>,
        > => {
          Array.mapWithIndex(ts, (i, t) => {
            let p = Array.concat(pos, [i])
            if markLen == Array.length(p) {
              if p == mark {
                cursor[markLen - 1]
              } else if p == cursor {
                mark[markLen - 1]
              } else {
                None
              }
              ->Option.flatMap(c => ts[c])
              ->Option.getWithDefault(t)
            } else {
              {
                label: t.label,
                forest: traverse(t.forest, p),
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
                forest: traverse(body.forest, [0]),
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
  open Belt
  Array.length(a) <= Array.length(b) &&
    Array.reduceWithIndex(a, true, (acm, p, i) => {
      acm && p === Array.getUnsafe(b, i)
    })
}

let reducer = (state, action) => {
  open Belt
  Js.log(action)
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
  | Append => {
      let {selection, container} = state
      switch selection {
      | SchemaSelection({cursor}) =>
        switch container {
        | Enumerable({schema: {id, name, body}, value}) => {
            ...state,
            container: Enumerable({
              value: value,
              schema: {
                id: id,
                name: name,
                body: Tree.map(body, (p, t) =>
                  p == cursor ? [t, t] : [t]
                )[0]->Option.getWithDefault(body),
              },
            }),
          }
        | _ => state
        }
      | ValueSelection(_) => state
      }
    }
  | Edit(position, editValue) => {
      let {container} = state
      switch container {
      | Enumerable({schema: {id, name, body}, value}) => {
          ...state,
          container: Enumerable({
            value: value,
            schema: {
              id: id,
              name: name,
              body: Tree.map(body, (p, t) =>
                p == position ? [{...t, label: editValue}] : [t]
              )[0]->Option.getWithDefault(body),
            },
          }),
        }
      | _ => state
      }
    }
  | ChangeMode(mode) => {
      ...state,
      mode: mode,
    }
  }
}

module SchemaText = {
  @react.component
  let make = (~label: string) => {
    <div> {React.string(label)} <span> {React.string("(number)")} </span> </div>
  }
}

module EditArea = {
  open Belt
  @react.component
  let make = (~value: string, ~onChange: string => unit) => {
    let handleEdit = React.useCallback1(ev => {
      ReactEvent.Form.stopPropagation(ev)
      ReactEvent.Form.target(ev)["value"]->onChange
    }, [onChange])
    let ref = React.useRef(Js.Nullable.null)
    React.useEffect0(() => {
      ref.current->Js.Nullable.toOption->Option.forEach(i => i->focus)
      None
    })
    <input onChange={handleEdit} value={value} ref={ReactDOM.Ref.domRef(ref)} />
  }
}

module SchemaLabel = {
  @react.component
  let make = (
    ~label: schemaBody,
    ~position: array<int>,
    ~selection: option<schemaSelection>,
    ~edit: React.callback<(array<int>, schemaBody), unit>,
    ~mode: mode,
  ) => {
    open Belt
    let schemaLabel = switch label {
    | SText(label) => label
    | SNumber({label}) => label
    }
    let handleEdit = React.useCallback3(value => {
      Js.log(value)
      edit((
        position,
        switch label {
        | SText(_) => SText(value)
        | SNumber({unit}) => SNumber({label: value, unit: unit})
        },
      ))
    }, (edit, label, position))
    let (isSelected, isMarked) = switch selection {
    | Some({cursor, mark}) => (position == cursor, position == mark)
    | _ => (false, false)
    }

    <>
      {Array.reduce(position, "", (acm, p) => acm ++ Int.toString(p))->React.string}
      {switch mode {
      | Edit if isSelected => <EditArea value={schemaLabel} onChange={handleEdit} />
      | _ =>
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
          | SNumber({label, unit: {id: _id, name: _name}}) => <SchemaText label={label} />
          | SText(label) =>
            <div> {React.string(label)} <span> {React.string("(label)")} </span> </div>
          }}
        </div>
      }}
    </>
  }
}
type schemaBlockProps = {
  tree: tree<schemaBody>,
  selection: option<schemaSelection>,
  position: array<int>,
  edit: React.callback<(array<int>, schemaBody), unit>,
  mode: mode,
}

module type SchemaBlock = {
  type props = schemaBlockProps
  @obj
  external makeProps: (
    ~tree: tree<schemaBody>,
    ~selection: option<schemaSelection>=?,
    ~position: array<int>,
    ~edit: React.callback<(array<int>, schemaBody), unit>,
    ~mode: mode,
    ~key: string=?,
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
    ~edit: React.callback<(array<int>, schemaBody), unit>,
    ~mode: mode,
    ~key: string=?,
    unit,
  ) => props = ""

  let make = ({tree, selection, position, edit, mode}) => {
    <>
      <SchemaLabel label={tree.label} position selection edit mode />
      {Belt.Array.mapWithIndex(tree.forest, (i, f) => {
        let pos = Belt.Array.concat(position, [i])
        let key = Belt.Array.reduce(pos, "", (acm, p) => `${acm} ${p->Belt.Int.toString}`)
        <SchemaBlock tree={f} selection position={pos} edit mode key />
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
    keySetting: getKeyboardSetting(),
  },
  mode: Normal,
}

module DownloadButton = {
  open Belt
  @react.component
  let make = (~content: state) => {
    let anchor = React.useRef(Js.Nullable.null)
    <>
      <button
        onClick={_ => {
          anchor.current->Js.Nullable.toOption->Option.forEach(a => a->click)
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
      <input type_={"file"} accept={".json, .text, .txt"} ref={ReactDOM.Ref.domRef(inputRef)} />
    </>
  }
}

module App = {
  open Belt
  @react.component
  let make = () => {
    let (state, dispatch) = React.useReducer(reducer, mock)
    let {selection, container, setting: {keySetting}, mode} = state
    let ({body}, value) = switch container {
    | Enumerable({schema, value}) => (schema, value)
    | Transitional({schema, value}) => (schema, value)
    | MasterSlave({schema, value}) => (schema, [value.master])
    }

    let edit = React.useCallback1(((pos: array<int>, schemaBody)) => {
      dispatch(Edit(pos, schemaBody))
    }, [dispatch])

    React.useEffect1(() => {
      let {
        keyDown,
        keyUp,
        keyLeft,
        keyRight,
        keyEnter,
        keyAppend,
        keyScrollUp,
        keyScrollDown,
        keyEdit,
        escape,
      } = getKeyboardSetting(~ks=keySetting, ())
      let onInput = (inp: keySet, s: array<keySet>, fn: unit => unit) => {
        let r = Array.some(s, k => eq(k, inp))
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

        Array.some(
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
            keyAppend->handle(() => {
              dispatch(Append)
            }),
            keyScrollDown->handle(
              %raw(`() => {
                scrollBy(0, innerHeight)
            }`),
            ),
            keyScrollUp->handle(
              %raw(`() => {
                scrollBy(0, -innerHeight)
            }`),
            ),
            keyEdit->handle(() => {
              dispatch(ChangeMode(Edit))
            }),
            escape->handle(() => {
              dispatch(ChangeMode(Normal))
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
      <SchemaBlock tree={body} selection={schemaSelection} position={[0]} edit mode />
      <ValueArray value />
      <div
        style={ReactDOM.Style.make(
          ~position="fixed",
          ~bottom="0",
          ~right="0",
          ~display="flex",
          ~flexDirection="column",
          (),
        )}>
        <UploadButton _upload_={state => dispatch(Load(state))} />
        <DownloadButton content={state} />
      </div>
    </div>
  }
}

@react.component
let make = () => {
  <App />
}
