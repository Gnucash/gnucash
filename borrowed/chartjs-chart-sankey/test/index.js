import {
  acquireChart,
  addMatchers,
  afterEvent,
  releaseCharts,
  specsFromFixtures,
  triggerMouseEvent,
} from 'chartjs-test-utils'

window.devicePixelRatio = 1
window.acquireChart = acquireChart
window.afterEvent = afterEvent
window.triggerMouseEvent = triggerMouseEvent

jasmine.fixtures = specsFromFixtures

beforeEach(function () {
  addMatchers()
})

afterEach(function () {
  releaseCharts()
})
