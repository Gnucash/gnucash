const data = [
  { flow: 6, from: 'trailer-2', to: 'trailer-3' },
  { flow: 39, from: 'film-1', to: 'tv_show-2' },
  { flow: 12, from: 'trailer-1', to: 'tv_show-2' },
  { flow: 14, from: 'trailer-2', to: 'film-3' },
  { flow: 13, from: 'tv_show-1', to: 'trailer-2' },
  { flow: 12, from: 'film-2', to: 'trailer-3' },
  { flow: 27, from: 'film-2', to: 'film-3' },
  { flow: 12, from: 'tv_show-2', to: 'trailer-3' },
  { flow: 35, from: 'tv_show-1', to: 'film-2' },
  { flow: 12, from: 'trailer-2', to: 'tv_show-3' },
  { flow: 33, from: 'tv_show-2', to: 'film-3' },
  { flow: 13, from: 'film-1', to: 'trailer-2' },
  { flow: 8, from: 'trailer-1', to: 'trailer-2' },
  { flow: 43, from: 'film-2', to: 'tv_show-3' },
  { flow: 30, from: 'tv_show-1', to: 'tv_show-2' },
  { flow: 14, from: 'trailer-1', to: 'film-2' },
  { flow: 33, from: 'tv_show-2', to: 'tv_show-3' },
  { flow: 37, from: 'film-1', to: 'film-2' },
]

module.exports = {
  tolerance: 0.02,
  config: {
    type: 'sankey',
    data: {
      datasets: [
        {
          data,
          colorFrom: 'blue',
          colorTo: 'green',
        },
      ],
    },
  },
  options: {
    spriteText: true,
    canvas: {
      height: 256,
      width: 512,
    },
  },
}
