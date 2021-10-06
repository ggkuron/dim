open Belt
open Test
open Application

type rec matchers<'a> = {
  toBe: 'a => unit,
  toEqual: 'a => unit,
  not: unit => matchers<'a>,
}

let expect = (~message: option<string>=?, actual: unit => 'a) => {
  let generateMessage = actual => {
    let msg =
      Js.String2.make(actual)
      ->Js.String2.unsafeReplaceBy1(%re(`/function\b[^{]+{((?:.|\n|\r)*)}/`), (
        _match,
        p1,
        _offset,
        _wholeString,
      ) => p1)
      ->Js.String2.unsafeReplaceBy1(%re(`/return(?:\s|\\r|\\n|\r|\n)+((?:.|\n|\r)*)/`), (
        _match,
        p1,
        _offset,
        _wholeString,
      ) => p1)
      ->Js.String2.split("\n")
      ->Array.reduce("", (acm, p) => acm ++ Js.String2.trim(p))
    message->Option.map(m => m ++ "\n")->Option.getWithDefault(msg)
  }
  let rec matchers = (negated: bool): matchers<'a> => {
    {
      toBe: right => {
        let left = actual()
        let r = left === right
        assertion(
          ~message=generateMessage(actual),
          ~operator="toBe",
          (_, _) => negated ? !r : r,
          left,
          right,
        )
      },
      toEqual: right => {
        let left = actual()
        let r = left == right
        assertion(
          ~message=generateMessage(actual),
          ~operator="toEqual",
          (_, _) => negated ? !r : r,
          left,
          right,
        )
      },
      not: () => {
        matchers(true)
      },
    }
  }
  matchers(false)
}
test("matchers", () => {
  expect(() => 1).toBe(1)
  expect(() => 1).not().toBe(2)
  expect(() => "foo").toBe("foo")
  expect(() => "foo").not().toBe("bar")
  expect(() => []).not().toBe([])
  expect(() => 1).toEqual(1)
  expect(() => 1).not().toEqual(2)
  expect(() => [1]).toEqual([1])
  expect(() => [1]).not().toEqual([2])

  expect(() => []).toEqual([])
  expect(() => [1]).toEqual([1])
  expect(() => [1]).not().toEqual([0])
})

test("isFilled", () => {
  expect(() => isFilled([], [])).toBe(true)
  expect(() => isFilled([1], [])).not().toBe(true)
  expect(() => isFilled([], [1])).toBe(true)
  expect(() => isFilled([1], [1])).toBe(true)
  expect(() => isFilled([1], [2])).toBe(false)
  expect(() => isFilled([1, 2], [1, 2])).toBe(true)
  expect(() => isFilled([1, 2], [1])).toBe(false)
  expect(() => isFilled([1, 2], [1, 3])).toBe(false)
  expect(() => isFilled([1, 2], [1, 3, 2])).toBe(false)
  expect(() => isFilled([1, 2], [1, 2, 2])).toBe(true)
})

open Tree

test("tree", () => {
  expect(() =>
    {
      label: [],
      forest: [],
    }->get([])
  ).toEqual(Some([]))
  expect(() =>
    {
      label: [],
      forest: [],
    }->get([0])
  ).toEqual(None)
  expect(() =>
    {
      label: [0],
      forest: [],
    }->get([1])
  ).toEqual(None)
  expect(() =>
    {
      label: [0],
      forest: [],
    }->get([0, 0])
  ).toEqual(None)
  expect(() =>
    {
      label: [0],
      forest: [
        {
          label: [0, 1],
          forest: [],
        },
      ],
    }->get([0])
  ).toEqual(Some([0, 1]))
  expect(() =>
    {
      label: [0],
      forest: [
        {
          label: [0, 1],
          forest: [],
        },
      ],
    }->get([0, 1])
  ).toEqual(None)
})

test("Tree.map", () => {
  expect(() =>
    {
      label: 1,
      forest: [],
    }->map((_, t) => [
      {
        ...t,
        label: t.label * 2,
      },
    ])
  ).toEqual([
    {
      label: 2,
      forest: [],
    },
  ])
  let l3tree = {
    label: 1,
    forest: [
      {
        label: 2,
        forest: [],
      },
      {
        label: 3,
        forest: [
          {
            label: 4,
            forest: [],
          },
        ],
      },
    ],
  }
  expect(() =>
    l3tree->map((_, t) => [
      {
        ...t,
        label: t.label * 2,
      },
    ])
  ).toEqual([
    {
      label: 2,
      forest: [
        {
          label: 4,
          forest: [],
        },
        {
          label: 6,
          forest: [
            {
              label: 8,
              forest: [],
            },
          ],
        },
      ],
    },
  ])
  expect(() =>
    l3tree->map((p, t) => [
      p == [0, 0]
        ? {
            ...t,
            label: 5,
          }
        : t,
    ])
  ).toEqual([
    {
      label: 1,
      forest: [
        {
          label: 5,
          forest: [],
        },
        {
          label: 3,
          forest: [
            {
              label: 4,
              forest: [],
            },
          ],
        },
      ],
    },
  ])
  expect(() => l3tree->map((p, t) => p == [0, 0] ? [t, t] : [t])).toEqual([
    {
      label: 1,
      forest: [
        {
          label: 2,
          forest: [],
        },
        {
          label: 2,
          forest: [],
        },
        {
          label: 3,
          forest: [
            {
              label: 4,
              forest: [],
            },
          ],
        },
      ],
    },
  ])
})
