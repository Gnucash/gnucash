const data = [
  { flow: 19, from: 'trailer-2', to: 'trailer-3' },
  { flow: 93, from: 'film-1', to: 'tv_show-2' },
  { flow: 44, from: 'trailer-1', to: 'tv_show-2' },
  { flow: 36, from: 'trailer-2', to: 'film-3' },
  { flow: 32, from: 'tv_show-1', to: 'trailer-2' },
  { flow: 41, from: 'film-2', to: 'trailer-3' },
  { flow: 83, from: 'film-2', to: 'film-3' },
  { flow: 44, from: 'tv_show-2', to: 'trailer-3' },
  { flow: 87, from: 'tv_show-1', to: 'film-2' },
  { flow: 30, from: 'trailer-2', to: 'tv_show-3' },
  { flow: 37, from: 'film-1', to: 'trailer-2' },
  { flow: 78, from: 'tv_show-2', to: 'film-3' },
  { flow: 18, from: 'trailer-1', to: 'trailer-2' },
  { flow: 93, from: 'film-2', to: 'tv_show-3' },
  { flow: 76, from: 'tv_show-1', to: 'tv_show-2' },
  { flow: 46, from: 'trailer-1', to: 'film-2' },
  { flow: 88, from: 'tv_show-2', to: 'tv_show-3' },
  { flow: 88, from: 'film-1', to: 'film-2' },
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
