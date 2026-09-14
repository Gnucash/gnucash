import { describe, expect, test } from '@jest/globals'

import { defined, toTextLines, validateSizeValue } from './helpers'

describe('lib/helpers', () => {
  describe('defined', () => {
    test.each([
      [false, undefined],
      [true, null],
      [true, 0],
      [true, 'abba'],
      [true, []],
      [true, {}],
      [true, NaN],
    ])('should return %p for %p', (expected, input) => {
      expect(defined(input)).toBe(expected)
    })
  })
  describe('toTxtLines', () => {
    test.each([
      [[], undefined],
      [['123'], 123],
      [['test'], 'test'],
      [['test'], ['test']],
      [['a', 'b'], 'a\nb'],
      [['a', 'b'], ['a\nb']],
      [
        ['a', 'b', 'c', 'd', 'e', 'f', 'g', '1', '2', '3'],
        ['a\nb', ['c', 'd', 'e\nf\ng'], 1, [2, 3]],
      ],
    ])('should return %p when input is %p', (expected, input) => {
      expect(toTextLines(input)).toEqual(expected)
    })
  })
  describe('validateSizeValue', () => {
    test.each([
      ['min', 'min'],
      ['max', 'max'],
      ['max', 'foo'],
      ['max', undefined],
    ])('should return %p when input is %p', (expected, input) => {
      expect(validateSizeValue(input)).toEqual(expected)
    })
  })
})
