import {
  CartesianScaleTypeRegistry,
  Chart,
  ChartComponent,
  Color,
  DatasetController,
  Element,
  FontSpec,
  Scriptable,
  ScriptableContext,
  VisualElement,
} from 'chart.js'

type AnyObject = Record<string, unknown>

declare module 'chart.js' {
  /* raw data element */
  interface SankeyDataPoint {
    from: string
    to: string
    flow: number
  }

  /* dataset configuration */
  interface SankeyControllerDatasetOptions {
    label: string
    data: Array<SankeyDataPoint>
    colorFrom: (data: ScriptableContext<'sankey'>) => string
    colorTo: (data: ScriptableContext<'sankey'>) => string
    colorMode: 'gradient' | 'from' | 'to'
    hoverColorFrom?: Scriptable<string, ScriptableContext<'sankey'>>
    hoverColorTo?: Scriptable<string, ScriptableContext<'sankey'>>
    /* Map<node.key, priority_value> */
    priority?: Record<string, number>
    column?: Record<string, number>
    /* Map<node.key, label> */
    labels?: Record<string, string>

    modeX?: 'edge' | 'even' /* defaults to 'edge' */
    size?: 'min' | 'max' /* defaults to 'max' */
    borderWidth?: number /* defaults to 1 */
    nodeWidth?: number /* defaults to 10 */
    nodePadding?: number /* defaults to 10 (pixels) */
    color?: string /* defaults to 'black' */
    borderColor?: string /* defaults to 'black' */
    font?: FontSpec /* defaults to chart.options.font */
    padding?: number /* defaults to font.lineHeight / 2 */

    parsing: { from: string; to: string; flow: string }
  }

  type FromToElement = {
    addY: number
    flow: number
    key: string
    node: SankeyNode
    index: number
  }

  type SankeyNode = {
    /* unique key of a node */
    key: string
    /* number of => in-connections */
    in: number
    /* number of out => connections */
    out: number
    /* node size, based on size option */
    size: number
    from: Array<FromToElement>
    to: Array<FromToElement>
    /* true if x is defined by SankeyControllerDatasetOptions.column map  */
    column?: boolean
    /* priority extracted from the SankeyControllerDatasetOptions.priority map */
    priority?: number
    y?: number
    x?: number
    color?: Color
    /** internal */
    _visited?: number
  }

  interface SankeyParsedData {
    x: number
    y: number
    _custom: {
      from: SankeyNode
      to: SankeyNode
      x: number
      y: number
      height: number
      flow: number
    }
  }

  interface ChartTypeRegistry {
    sankey: {
      datasetOptions: SankeyControllerDatasetOptions
      defaultDataPoint: SankeyDataPoint
      parsedDataType: SankeyParsedData
      metaExtensions: AnyObject
      /* TODO: define sankey chart options */
      chartOptions: FlowOptions
      scales: keyof CartesianScaleTypeRegistry
    }
  }
}

export interface FlowProps {
  x: number
  y: number
  x2: number
  y2: number
  height: number
  width: number
}

export interface FlowOptions {
  alpha: number
  colorMode: 'gradient' | 'from' | 'to'
  colorFrom: 'string'
  colorTo: Color
  hoverColorFrom: Color
  hoverColorTo: Color
}

export interface FlowConfig {
  x: number
  y: number
  x2: number
  y2: number
  height: number
  options: FlowOptions
}

export type SankeyController = DatasetController
export const SankeyController: ChartComponent & {
  prototype: SankeyController
  new (chart: Chart, datasetIndex: number): SankeyController
}

export interface Flow<T extends FlowConfig = FlowConfig, O extends FlowOptions = FlowOptions>
  extends Element<T, O>,
    VisualElement {}

export const Flow: ChartComponent & {
  prototype: Flow
  new (cfg: FlowConfig): Flow
}
