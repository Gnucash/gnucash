import {
  ChartMeta,
  DatasetController,
  FromToElement,
  SankeyControllerDatasetOptions,
  SankeyNode,
  SankeyParsedData,
} from 'chart.js'
import { toFont, valueOrDefault } from 'chart.js/helpers'

import { AnyObject } from '../types/index.esm'

import { buildNodesFromData, getParsedData } from './lib/core'
import { toTextLines, validateSizeValue } from './lib/helpers'
import { layout } from './lib/layout'
import Flow from './flow'

function getAddY(arr: FromToElement[], key: string, index: number): number {
  for (const item of arr) {
    if (item.key === key && item.index === index) {
      return item.addY
    }
  }
  return 0
}

export default class SankeyController extends DatasetController {
  static readonly id = 'sankey'

  static readonly defaults = {
    dataElementType: 'flow',
    animations: {
      numbers: {
        type: 'number',
        properties: ['x', 'y', 'x2', 'y2', 'height'],
      },
      progress: {
        easing: 'linear',
        duration: (ctx) => (ctx.type === 'data' ? (ctx.parsed._custom.x - ctx.parsed.x) * 200 : undefined),
        delay: (ctx) => (ctx.type === 'data' ? ctx.parsed.x * 500 + ctx.dataIndex * 20 : undefined),
      },
      colors: {
        type: 'color',
        properties: ['colorFrom', 'colorTo'],
      },
    },
    color: 'black',
    borderColor: 'black',
    borderWidth: 1,
    modeX: 'edge',
    nodeWidth: 10,
    nodePadding: 10,
    transitions: {
      hide: {
        animations: {
          colors: {
            type: 'color',
            properties: ['colorFrom', 'colorTo'],
            to: 'transparent',
          },
        },
      },
      show: {
        animations: {
          colors: {
            type: 'color',
            properties: ['colorFrom', 'colorTo'],
            from: 'transparent',
          },
        },
      },
    },
  }

  static readonly overrides = {
    interaction: {
      mode: 'nearest',
      intersect: true,
    },
    datasets: {
      clip: false,
      parsing: { from: 'from', to: 'to', flow: 'flow' },
    },
    plugins: {
      tooltip: {
        callbacks: {
          title() {
            return ''
          },
          label(context) {
            const parsedCustom = context.parsed._custom
            return parsedCustom.from.key + ' -> ' + parsedCustom.to.key + ': ' + parsedCustom.flow
          },
        },
      },
      legend: {
        display: false,
      },
    },
    scales: {
      x: {
        type: 'linear',
        bounds: 'data',
        display: false,
        min: 0,
        offset: false,
      },
      y: {
        type: 'linear',
        bounds: 'data',
        display: false,
        min: 0,
        reverse: true,
        offset: false,
      },
    },
    layout: {
      padding: {
        top: 3,
        left: 3,
        right: 13,
        bottom: 3,
      },
    },
  }

  options: SankeyControllerDatasetOptions
  private _nodes: Map<string, SankeyNode>
  private _maxX: number
  private _maxY: number

  override parseObjectData(
    meta: ChartMeta<'sankey', Flow>,
    data: AnyObject[],
    start: number,
    count: number
  ): SankeyParsedData[] {
    const sankeyData = getParsedData(data, this.options.parsing)
    const { xScale, yScale } = meta
    const parsed: SankeyParsedData[] = []
    const nodes = (this._nodes = buildNodesFromData(sankeyData, this.options))

    const { maxX, maxY } = layout(nodes, sankeyData, {
      priority: !!this.options.priority,
      height: this.chart.canvas.height,
      nodePadding: this.options.nodePadding,
      modeX: this.options.modeX,
    })

    this._maxX = maxX
    this._maxY = maxY

    if (!xScale || !yScale) return []

    for (let i = 0, ilen = sankeyData.length; i < ilen; ++i) {
      const dataPoint = sankeyData[i]
      const from = nodes.get(dataPoint.from)
      const to = nodes.get(dataPoint.to)
      if (!from || !to) continue

      const fromY: number = (from.y ?? 0) + getAddY(from.to, dataPoint.to, i)
      const toY: number = (to.y ?? 0) + getAddY(to.from, dataPoint.from, i)

      parsed.push({
        x: xScale.parse(from.x, i) as number,
        y: yScale.parse(fromY, i) as number,
        _custom: {
          from,
          to,
          x: xScale.parse(to.x, i) as number,
          y: yScale.parse(toY, i) as number,
          height: yScale.parse(dataPoint.flow, i) as number,
          flow: dataPoint.flow,
        },
      })
    }
    return parsed.slice(start, start + count)
  }

  override getMinMax(scale) {
    return {
      min: 0,
      max: scale === this._cachedMeta.xScale ? this._maxX : this._maxY,
    }
  }

  override update(mode) {
    const { data } = this._cachedMeta as ChartMeta<'sankey', Flow>

    this.updateElements(data, 0, data.length, mode)
  }

  override updateElements(
    elems: Flow[],
    start: number,
    count: number,
    mode: 'default' | 'resize' | 'reset' | 'none' | 'hide' | 'show' | 'active'
  ) {
    const { xScale, yScale } = this._cachedMeta
    if (!xScale || !yScale) return

    const firstOpts = this.resolveDataElementOptions(start, mode)
    const sharedOptions = this.getSharedOptions(firstOpts)
    const { borderWidth, nodeWidth = 10 } = this.options
    const borderSpace = borderWidth ? borderWidth / 2 + 0.5 : 0

    for (let i = start; i < start + count; i++) {
      const parsed = this.getParsed(i) as SankeyParsedData
      const custom = parsed._custom
      const y = yScale.getPixelForValue(parsed.y)
      this.updateElement(
        elems[i],
        i,
        {
          x: xScale.getPixelForValue(parsed.x) + nodeWidth + borderSpace,
          y,
          x2: xScale.getPixelForValue(custom.x) - borderSpace,
          y2: yScale.getPixelForValue(custom.y),
          from: custom.from,
          to: custom.to,
          progress: mode === 'reset' ? 0 : 1,
          height: Math.abs(yScale.getPixelForValue(parsed.y + custom.height) - y),
          options: this.resolveDataElementOptions(i, mode),
        },
        mode
      )
    }

    this.updateSharedOptions(sharedOptions, mode, firstOpts)
  }

  private _drawLabels() {
    const ctx = this.chart.ctx
    const options = this.options
    const nodes = this._nodes || new Map()
    const size = validateSizeValue(options.size)
    const borderWidth = options.borderWidth ?? 1
    const nodeWidth = options.nodeWidth ?? 10
    const labels = options.labels
    const { xScale, yScale } = this._cachedMeta

    if (!xScale || !yScale) return

    ctx.save()
    const chartArea = this.chart.chartArea
    for (const node of nodes.values()) {
      const x = xScale.getPixelForValue(node.x)
      const y = yScale.getPixelForValue(node.y)

      const max = Math[size](node.in || node.out, node.out || node.in)
      const height = Math.abs(yScale.getPixelForValue(node.y + max) - y)
      const label = labels?.[node.key] ?? node.key
      let textX = x
      ctx.fillStyle = options.color ?? 'black'
      ctx.textBaseline = 'middle'
      if (x < chartArea.width / 2) {
        ctx.textAlign = 'left'
        textX += nodeWidth + borderWidth + 4
      } else {
        ctx.textAlign = 'right'
        textX -= borderWidth + 4
      }
      this._drawLabel(label, y, height, ctx, textX)
    }
    ctx.restore()
  }

  private _drawLabel(label: string, y: number, height: number, ctx: CanvasRenderingContext2D, textX: number) {
    const font = toFont(this.options.font, this.chart.options.font)
    const lines = toTextLines(label)
    const lineCount = lines.length
    const middle = y + height / 2
    const textHeight = font.lineHeight
    const padding = valueOrDefault(this.options.padding, textHeight / 2)

    ctx.font = font.string

    if (lineCount > 1) {
      const top = middle - (textHeight * lineCount) / 2 + padding
      for (let i = 0; i < lineCount; i++) {
        ctx.fillText(lines[i], textX, top + i * textHeight)
      }
    } else {
      ctx.fillText(label, textX, middle)
    }
  }

  private _drawNodes() {
    const ctx = this.chart.ctx
    const nodes = this._nodes || new Map()
    const { borderColor, borderWidth = 0, nodeWidth = 10, size } = this.options
    const sizeMethod = validateSizeValue(size)
    const { xScale, yScale } = this._cachedMeta

    ctx.save()
    if (borderColor && borderWidth) {
      ctx.strokeStyle = borderColor
      ctx.lineWidth = borderWidth
    }

    for (const node of nodes.values()) {
      ctx.fillStyle = node.color ?? 'black'
      const x = xScale!.getPixelForValue(node.x)
      const y = yScale!.getPixelForValue(node.y)

      const max = Math[sizeMethod](node.in || node.out, node.out || node.in)
      const height = Math.abs(yScale!.getPixelForValue(node.y + max) - y)
      if (borderWidth) {
        ctx.strokeRect(x, y, nodeWidth, height)
      }
      ctx.fillRect(x, y, nodeWidth, height)
    }
    ctx.restore()
  }

  /**
   * That's where the drawing process happens
   */
  override draw() {
    const ctx = this.chart.ctx
    const data = (this.getMeta().data as Flow[]) ?? []

    // Set node colors
    const active: Flow[] = []
    for (let i = 0, ilen = data.length; i < ilen; ++i) {
      const flow = data[i] /* Flow at index i */
      flow.from.color = flow.options.colorFrom
      flow.to.color = flow.options.colorTo
      if (flow.active) {
        active.push(flow)
      }
    }

    // Make sure nodes connected to hovered flows are using hover colors.
    for (const flow of active) {
      flow.from.color = flow.options.colorFrom
      flow.to.color = flow.options.colorTo
    }

    this._drawNodes()

    for (let i = 0, ilen = data.length; i < ilen; ++i) {
      data[i].draw(ctx)
    }

    this._drawLabels()
  }
}
