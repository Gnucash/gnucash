import { describe, expect, test } from '@jest/globals'
import { SankeyDataPoint } from 'chart.js'

import { buildNodesFromData } from './core'

describe('lib/core', () => {
  describe('buildNodesFromData', () => {
    test('it should build nodes from simple flows', () => {
      const data: SankeyDataPoint[] = [{ from: 'a', to: 'b', flow: 1 }]

      const nodes = buildNodesFromData(data, {})

      expect(nodes.size).toEqual(2)
      expect([...nodes.keys()]).toEqual(['a', 'b'])
      expect(nodes.get('a')).toEqual({
        from: [],
        in: 0,
        key: 'a',
        out: 1,
        size: 1,
        to: [
          {
            addY: 0,
            flow: 1,
            index: 0,
            key: 'b',
            node: expect.any(Object), // circular
          },
        ],
      })
      expect(nodes.get('b')).toEqual({
        from: [
          {
            addY: 0,
            flow: 1,
            index: 0,
            key: 'a',
            node: expect.any(Object), // circular
          },
        ],
        in: 1,
        key: 'b',
        out: 0,
        size: 1,
        to: [],
      })
    })

    test('should build nodes from comples flows', () => {
      const data = [
        { from: 'Coal imports', to: 'Coal', flow: 11.606 },
        { from: 'Coal reserves', to: 'Coal', flow: 63.965 },
        { from: 'Coal', to: 'Solid', flow: 75.571 },
        { from: 'Bio-conversion', to: 'Solid', flow: 280.322 },
        { from: 'Biomass imports', to: 'Solid', flow: 35 },
        { from: 'Other waste', to: 'Solid', flow: 56.587 },
        { from: 'Solid', to: 'Agriculture', flow: 0.882 },
      ]

      const nodes = buildNodesFromData(data, {})

      expect(nodes.size).toEqual(8)

      expect(nodes.get('Coal imports')).toEqual(expect.objectContaining({ in: 0, out: 11.606 }))
      expect(nodes.get('Coal reserves')).toEqual(expect.objectContaining({ in: 0, out: 63.965 }))
      expect(nodes.get('Coal')).toEqual(expect.objectContaining({ in: 75.571, out: 75.571 }))
      expect(nodes.get('Bio-conversion')).toEqual(expect.objectContaining({ in: 0, out: 280.322 }))
      expect(nodes.get('Biomass imports')).toEqual(expect.objectContaining({ in: 0, out: 35 }))
      expect(nodes.get('Other waste')).toEqual(expect.objectContaining({ in: 0, out: 56.587 }))
      expect(nodes.get('Solid')).toEqual(expect.objectContaining({ in: 447.48, out: 0.882 }))
      expect(nodes.get('Agriculture')).toEqual(expect.objectContaining({ in: 0.882, out: 0 }))
    })

    test('it should support circular flows', () => {
      const data: SankeyDataPoint[] = [{ from: 'abba', to: 'abba', flow: 123.5 }]

      const nodes = buildNodesFromData(data, {})

      expect(nodes.size).toBe(1)
      expect(nodes.get('abba')).toEqual(
        expect.objectContaining({
          from: [expect.any(Object)], // circular
          in: 123.5,
          key: 'abba',
          out: 123.5,
          size: 123.5,
          to: [expect.any(Object)], // circular
        })
      )
    })

    test('it should include data with no flow', () => {
      const data: SankeyDataPoint[] = [
        { from: 'one', to: 'other', flow: 0 },
        { from: 'one', to: 'third', flow: 2 },
      ]

      const nodes = buildNodesFromData(data, {})

      expect(nodes.size).toBe(3)
      expect([...nodes.keys()]).toEqual(['one', 'other', 'third'])
    })

    test('it should sort flows', () => {
      const data: SankeyDataPoint[] = [
        { from: 'a1', to: 'b', flow: 2 },
        { from: 'a2', to: 'b', flow: 3 },
        { from: 'a3', to: 'b', flow: 1 },
        { from: 'a4', to: 'b', flow: 1 },
        { from: 'b', to: 'c1', flow: 3 },
        { from: 'b', to: 'c2', flow: 1 },
        { from: 'b', to: 'c3', flow: 2 },
        { from: 'b', to: 'c4', flow: 3 },
      ]

      const nodes = buildNodesFromData(data, {})

      expect(nodes.size).toBe(9)

      const b = nodes.get('b')

      expect(b.from.map(({ flow, index, key }) => ({ flow, index, key }))).toEqual([
        { flow: 3, index: 1, key: 'a2' },
        { flow: 2, index: 0, key: 'a1' },
        { flow: 1, index: 2, key: 'a3' },
        { flow: 1, index: 3, key: 'a4' },
      ])

      expect(b.to.map(({ flow, index, key }) => ({ flow, index, key }))).toEqual([
        { flow: 3, index: 4, key: 'c1' },
        { flow: 3, index: 7, key: 'c4' },
        { flow: 2, index: 6, key: 'c3' },
        { flow: 1, index: 5, key: 'c2' },
      ])
    })
  })
})
