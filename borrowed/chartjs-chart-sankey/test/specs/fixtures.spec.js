import { Chart } from 'chart.js'

describe('fixtures', jasmine.fixtures(''))

describe('index', function () {
  it('should register controller and element', function () {
    expect(Chart.registry.getController('sankey')).toBeDefined()
    expect(Chart.registry.getElement('flow')).toBeDefined()
  })
})
