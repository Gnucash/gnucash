import type { FromToElement, SankeyControllerDatasetOptions, SankeyDataPoint, SankeyNode } from 'chart.js'

import { defined } from './helpers'

const SMALL_VALUE = 1e-6

export type SankeyMode = 'edge' | 'even'

/**
 * Get all keys the input nodes flow to, including keys of the input nodes
 */
export const getAllKeysForward = (nodes: SankeyNode[], visited: Set<string> = new Set()): string[] => {
  const keys: string[] = []
  for (const node of nodes) {
    if (visited.has(node.key)) continue
    visited.add(node.key)
    keys.push(
      node.key,
      ...getAllKeysForward(
        node.to.map((to) => to.node),
        visited
      )
    )
  }

  return keys
}

/**
 * Find the nodes that should be placed leftmost on the chart.
 * NOTE: With circular flows, data order matters.
 */
export const startColumn = (data: SankeyDataPoint[], nodes: SankeyNode[]): string[] => {
  // First check if there are nodes without any input. Start from those.
  const startNodes = nodes.filter((node) => node.from.length === 0)
  const column = startNodes.map((node) => node.key)

  const startRef = getAllKeysForward(startNodes)

  // If there are no nodes without any inputs, this is a fully circular chart.
  // Build the start column based on data order and references.
  const referencedNodes = new Set(startRef)

  for (const point of data) {
    if (!referencedNodes.has(point.from) && !referencedNodes.has(point.to)) {
      column.push(point.from)
      referencedNodes.add(point.from)
    }
    referencedNodes.add(point.to)
  }
  return column
}

/**
 * Figure out the next column from remainingKeys
 * @param dataWithoutDirectLoops - data filtered so it does not contain direct loops (from === to)
 * @param remainingKeys - they keys that are not yet placed to the chart
 * @returns array of node keys to place in the next column
 */
const nextColumn = (dataWithoutDirectLoops: SankeyDataPoint[], remainingKeys: Set<string>): string[] => {
  const remainingTo = new Set(
    dataWithoutDirectLoops.filter((flow) => remainingKeys.has(flow.from)).map((flow) => flow.to)
  )
  const remainingKeyArray = [...remainingKeys]
  const columnsNotInTo = remainingKeyArray.filter((key) => !remainingTo.has(key))

  return columnsNotInTo.length ? columnsNotInTo : remainingKeyArray.slice(0, 1)
}

export function calculateX(nodeMap: Map<string, SankeyNode>, data: SankeyDataPoint[], mode: SankeyMode): number {
  const dataWithoutDirectLoops = data.filter((dp) => dp.from !== dp.to)
  const allKeys = [...nodeMap.keys()]
  const allNodes = [...nodeMap.values()]
  const keysToPlace = new Set(allKeys)
  let x = 0
  while (keysToPlace.size) {
    const column = x === 0 ? startColumn(data, allNodes) : nextColumn(dataWithoutDirectLoops, keysToPlace)

    if (!column.length) {
      // In case thre is a bug in column determination, throw an error instead of looping endlessly.
      throw new Error('Fatal error: Unable to place nodes to columns. Please report this issue.')
    }

    for (const key of column) {
      const node = nodeMap.get(key)
      if (node && !defined(node.x)) {
        node.x = x
      }
      keysToPlace.delete(key)
    }
    if (keysToPlace.size) {
      x++
    }
  }

  // Calculate the maxX from nodes in case some were placed by column option
  const maxX = allNodes.reduce((max, node) => Math.max(max, node.x), 0)

  if (mode === 'edge') {
    // Move nodes that have no output to the right edge of the flow
    const from = new Set(data.map((dataPoint) => dataPoint.from))
    allKeys
      .filter((key) => !from.has(key))
      .forEach((key) => {
        const node = nodeMap.get(key)
        // Only move the node to right edge, if it's column is not defined
        if (node && !node.column) {
          node.x = maxX
        }
      })
  }

  return maxX
}

// @todo: this will break when there are multiple charts
let prevCountId = -1
function getCountId() {
  prevCountId = prevCountId < 100 ? prevCountId + 1 : 0
  return prevCountId
}

function nodeCount(list: Array<FromToElement>, prop: string, countId = getCountId()): number {
  let count = 0
  for (const elem of list) {
    if (elem.node._visited === countId) {
      continue
    }
    elem.node._visited = countId
    count += elem.node[prop].length + nodeCount(elem.node[prop], prop, countId)
  }
  return count
}

const flowByNodeCount =
  (prop: string): ((a: FromToElement, b: FromToElement) => number) =>
  (a, b) =>
    nodeCount(a.node[prop], prop) - nodeCount(b.node[prop], prop) || a.node[prop].length - b.node[prop].length

function processFrom(node: SankeyNode, y: number): number {
  if (!node.from.length) return y

  node.from.sort(flowByNodeCount('from'))
  for (const flow of node.from) {
    const n = flow.node
    if (!defined(n.y)) {
      n.y = y
      processFrom(n, y ? y + SMALL_VALUE : 0)
    }
    y = Math.max(n.y + n.out, y)
  }
  return node.y + node.size
}

function processTo(node: SankeyNode, y: number): number {
  if (!node.to.length) return y

  node.to.sort(flowByNodeCount('to'))
  for (const flow of node.to) {
    const n = flow.node
    if (!defined(n.y)) {
      n.y = y
      processTo(n, y ? y + SMALL_VALUE : 0)
    }
    y = Math.max(n.y + Math.max(n.in, n.out), y)
  }
  return node.y + node.size
}

function setOrGetY(node: SankeyNode, value: number): number {
  if (defined(node.y)) {
    return node.y
  }
  node.y = value

  return value
}

function processRest(nodeArray: SankeyNode[], maxX: number) {
  const leftNodes = nodeArray.filter((node) => node.x === 0)
  const rightNodes = nodeArray.filter((node) => node.x === maxX)
  const leftToDo = leftNodes.filter((node) => !defined(node.y))
  const rightToDo = rightNodes.filter((node) => !defined(node.y))
  const centerToDo = nodeArray.filter((node) => node.x > 0 && node.x < maxX && !defined(node.y))

  let leftY = leftNodes.reduce((acc, cur) => Math.max(acc, cur.y + cur.out || 0), 0) + SMALL_VALUE
  let rightY = rightNodes.reduce((acc, cur) => Math.max(acc, cur.y + cur.in || 0), 0) + SMALL_VALUE
  let centerY = 0

  if (leftY >= rightY) {
    leftToDo.forEach((node) => {
      leftY = setOrGetY(node, leftY)
      leftY = Math.max(leftY + node.out, processTo(node, leftY))
    })

    rightToDo.forEach((node) => {
      rightY = setOrGetY(node, rightY)
      rightY = Math.max(rightY + node.in, processFrom(node, rightY))
    })
  } else {
    leftToDo.forEach((node) => {
      leftY = setOrGetY(node, leftY)
    })

    rightToDo.forEach((node) => {
      rightY = setOrGetY(node, rightY)
      rightY = Math.max(rightY + node.in, processFrom(node, rightY))
    })
  }
  centerToDo.forEach((node) => {
    let y = nodeArray
      .filter((n) => n.x === node.x && defined(n.y))
      .reduce((acc, cur) => Math.max(acc, cur.y + Math.max(cur.in, cur.out)), 0)
    y = setOrGetY(node, y)
    y = Math.max(y + node.in, processFrom(node, y))
    y = Math.max(y + node.out, processTo(node, y))
    centerY = Math.max(centerY, y)
  })

  return Math.max(leftY, rightY, centerY)
}

const fixTop = (nodeArray: SankeyNode[], maxX: number) => {
  let maxY = 0
  for (let x = 0; x <= maxX; x++) {
    const nodes = nodeArray.filter((n) => n.x === x).sort((a, b) => a.y - b.y)
    let minY = 0
    for (const node of nodes) {
      if (node.y < minY) node.y = minY
      minY = node.y + node.size
    }
    maxY = Math.max(maxY, minY)
  }
  return maxY
}

const findStartNode = (nodeArray: SankeyNode[], maxX: number): SankeyNode => {
  const size = [...nodeArray].sort((a, b) => a.size - b.size).pop().size
  const biggest = nodeArray.filter((n) => n.size === size)

  if (biggest.length === 1) return biggest[0]

  biggest.sort((a, b) => a.x - b.x)

  // if there is a big node at left edge, use it as starting point
  if (biggest[0].x === 0) return biggest[0]

  // same for right edge
  if (biggest[biggest.length - 1].x === maxX) return biggest.pop()

  // else start from center
  const mid = Math.floor(biggest.length / 2)
  return biggest[mid]
}

export function calculateY(nodeArray: SankeyNode[], maxX: number): number {
  if (!nodeArray.length) return 0

  const start = findStartNode(nodeArray, maxX)
  start.y = 0
  processFrom(start, 0)
  processTo(start, 0)
  processRest(nodeArray, maxX)
  return fixTop(nodeArray, maxX)
}

export function calculateYUsingPriority(nodeArray: SankeyNode[], maxX: number) {
  let maxY = 0
  let nextYStart = 0
  for (let x = 0; x <= maxX; x++) {
    let y = nextYStart
    const nodes = nodeArray.filter((node) => node.x === x).sort((a, b) => (a.priority ?? 0) - (b.priority ?? 0))
    nextYStart = nodes.length
      ? nodes[0].to.filter((to) => to.node.x > x + 1).reduce((acc, cur) => acc + cur.flow, 0) || 0
      : 0
    for (const node of nodes) {
      node.y = y
      y += Math.max(node.out, node.in)
    }
    maxY = Math.max(y, maxY)
  }
  return maxY
}

type NodeXYSize = Pick<SankeyNode, 'x' | 'y' | 'size'>
const nodeByXYSize = (a: NodeXYSize, b: NodeXYSize): number => {
  if (a.x !== b.x) return a.x - b.x
  if (a.y === b.y) return a.size - b.size
  return a.y - b.y
}

/**
 * @return {number} maxY
 */
export function addPadding(nodeArray: Pick<SankeyNode, 'x' | 'y' | 'in' | 'out' | 'size'>[], padding: number): number {
  let maxY = 0
  // const rows: number[] = [] // top left y of each row, exluding first row (y=0)
  const columnXs = new Map<number, number>()
  const grid: number[][] = []

  const getColIndex = (x: number) => {
    if (!columnXs.has(x)) {
      columnXs.set(x, grid.length)
      grid.push([])
    }
    return columnXs.get(x)
  }

  // sort nodes by x/y, so we can iterate them by rows
  nodeArray.sort(nodeByXYSize)

  for (const node of nodeArray) {
    const colIdx = getColIndex(node.x)
    const column = grid[colIdx]

    // figure out the max number of paddings in all columns above node.y
    if (node.y) {
      column.push(node.y)
      // Figure out the number of paddings needed. Start by the number of nodes above this in the same column.
      let paddings = column.length

      if (node.in) {
        // If the node has inputs, check all columsn left to this column and cound the nodes above this nodes y.
        // Use the maximun number of nodes above this node in any column left to it as number of paddings.
        for (let col = 0; col < colIdx; col++) {
          const otherColumn = grid[col]
          for (let row = 0; row < otherColumn.length; row++) {
            if (otherColumn[row] > node.y) break
            paddings = Math.max(row + 1, paddings)
          }
        }
        // update the column padding count by adding the same y multiple times if needed
        while (column.length < paddings) column.push(node.y)
      }

      // apply the paddings to the node
      node.y += paddings * padding
    }

    maxY = Math.max(maxY, node.y + Math.max(node.in, node.out))
  }

  return maxY
}

export function sortFlows(nodeArray: SankeyNode[]) {
  nodeArray.forEach((node) => {
    const nodeSize = node.size
    const overlapFrom = nodeSize < node.in
    const overlapTo = nodeSize < node.out
    let addY = 0
    let len = node.from.length
    node.from
      .sort((a, b) => a.node.y + a.node.out / 2 - (b.node.y + b.node.out / 2))
      .forEach((flow, idx) => {
        if (overlapFrom) {
          flow.addY = (idx * (nodeSize - flow.flow)) / (len - 1)
        } else {
          flow.addY = addY
          addY += flow.flow
        }
      })
    addY = 0
    len = node.to.length
    node.to
      .sort((a, b) => a.node.y + a.node.in / 2 - (b.node.y + b.node.in / 2))
      .forEach((flow, idx) => {
        if (overlapTo) {
          flow.addY = (idx * (nodeSize - flow.flow)) / (len - 1)
        } else {
          flow.addY = addY
          addY += flow.flow
        }
      })
  })
}

interface LayoutOptions {
  /** use node priority when sorting nodes vertically */
  priority: boolean
  /** canvas height (in pixels) */
  height: number
  /** vertical padding between nodes (in pixels) */
  nodePadding: number
  /** layout mode in x-direction */
  modeX: SankeyControllerDatasetOptions['modeX']
}

export function layout(
  nodes: Map<string, SankeyNode>,
  data: SankeyDataPoint[],
  { priority, height, nodePadding, modeX }: LayoutOptions
): { maxY: number; maxX: number } {
  const nodeArray = [...nodes.values()]
  const maxX = calculateX(nodes, data, modeX)
  const maxY = priority ? calculateYUsingPriority(nodeArray, maxX) : calculateY(nodeArray, maxX)
  const padding = (maxY / height) * nodePadding
  const maxYWithPadding = addPadding(nodeArray, padding)

  sortFlows(nodeArray)

  return { maxX, maxY: maxYWithPadding }
}
