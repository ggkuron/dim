open Test
open Application

type rec matchers<'a> = {
  toBe: 'a => unit,
  not: unit => matchers<'a>,
}

let expect = (~message: option<string>=?, actual: 'a) => {
  let rec matchers = (negated: bool): matchers<'a> => {
    {
      toBe: v => {
        assertion(
          ~message=switch message {
          | Some(m) => m ++ "\n"
          | None => ""
          } ++
          `Expected: ${Js.String2.make(v)}, ` ++
          `Actual: ${Js.String2.make(actual)}`,
          ~operator="toBe",
          (a, b) => {
            let r = a === b
            negated ? !r : r
          },
          actual,
          v,
        )
      },
      not: () => {
        matchers(true)
      },
    }
  }
  matchers(false)
}

test("isFilled", () => {
  expect(isFilled([], [])).toBe(true)
  expect(isFilled([1], [])).not().toBe(true)
  expect(isFilled([], [1])).toBe(true)
  expect(isFilled([1], [1])).toBe(true)
  expect(isFilled([1], [2])).toBe(false)
  expect(isFilled([1, 2], [1, 2])).toBe(true)
  expect(isFilled([1, 2], [1])).toBe(false)
  expect(isFilled([1, 2], [1, 3])).toBe(false)
  expect(isFilled([1, 2], [1, 3, 2])).toBe(false)
  expect(isFilled([1, 2], [1, 2, 2])).toBe(true)
})
