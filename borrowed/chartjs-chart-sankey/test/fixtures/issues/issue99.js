var colors2 = ['#fff5eb', '#fee6ce', '#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#d94801', '#a63603', '#7f2704']
var assigned = {}
function c2(name) {
  return assigned[name] || (assigned[name] = colors2[Object.keys(assigned).length % colors2.length])
}

module.exports = {
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data: [
            { from: 'Brazil', to: 'Portugal', flow: 5 },
            { from: 'Brazil', to: 'France', flow: 1 },
            { from: 'Brazil', to: 'Spain', flow: 1 },
            { from: 'Brazil', to: 'England', flow: 1 },
            { from: 'Canada', to: 'Portugal', flow: 1 },
            { from: 'Canada', to: 'France', flow: 5 },
            { from: 'Canada', to: 'England', flow: 1 },
            { from: 'Mexico', to: 'Portugal', flow: 1 },
            { from: 'Mexico', to: 'France', flow: 1 },
            { from: 'Mexico', to: 'Spain', flow: 5 },
            { from: 'Mexico', to: 'England', flow: 1 },
            { from: 'USA', to: 'Portugal', flow: 1 },
            { from: 'USA', to: 'France', flow: 1 },
            { from: 'USA', to: 'Spain', flow: 1 },
            { from: 'USA', to: 'England', flow: 5 },
            { from: 'Portugal', to: 'Angola', flow: 2 },
            { from: 'Portugal', to: 'Senegal', flow: 1 },
            { from: 'Portugal', to: 'Morocco', flow: 1 },
            { from: 'Portugal', to: 'South Africa', flow: 3 },
            { from: 'France', to: 'Angola', flow: 1 },
            { from: 'France', to: 'Senegal', flow: 3 },
            { from: 'France', to: 'Mali', flow: 3 },
            { from: 'France', to: 'Morocco', flow: 3 },
            { from: 'France', to: 'South Africa', flow: 1 },
            { from: 'Spain', to: 'Senegal', flow: 1 },
            { from: 'Spain', to: 'Morocco', flow: 3 },
            { from: 'Spain', to: 'South Africa', flow: 1 },
            { from: 'England', to: 'Angola', flow: 1 },
            { from: 'England', to: 'Senegal', flow: 1 },
            { from: 'England', to: 'Morocco', flow: 2 },
            { from: 'England', to: 'South Africa', flow: 7 },
            { from: 'South Africa', to: 'China', flow: 5 },
            { from: 'South Africa', to: 'India', flow: 1 },
            { from: 'South Africa', to: 'Japan', flow: 3 },
            { from: 'Angola', to: 'China', flow: 5 },
            { from: 'Angola', to: 'India', flow: 1 },
            { from: 'Angola', to: 'Japan', flow: 3 },
            { from: 'Senegal', to: 'China', flow: 5 },
            { from: 'Senegal', to: 'India', flow: 1 },
            { from: 'Senegal', to: 'Japan', flow: 3 },
            { from: 'Mali', to: 'China', flow: 5 },
            { from: 'Mali', to: 'India', flow: 1 },
            { from: 'Mali', to: 'Japan', flow: 3 },
            { from: 'Morocco', to: 'China', flow: 5 },
            { from: 'Morocco', to: 'India', flow: 1 },
            { from: 'Morocco', to: 'Japan', flow: 3 },
          ],
          colorFrom: (c) => c2(c.dataset.data[c.dataIndex].from),
          colorTo: (c) => c2(c.dataset.data[c.dataIndex].to),
          borderWidth: 0,
        },
      ],
    },
  },
  options: {
    spriteText: true,
    canvas: {
      height: 600,
      width: 800,
    },
  },
}
