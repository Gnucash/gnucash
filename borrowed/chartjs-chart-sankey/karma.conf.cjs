const istanbul = require('rollup-plugin-istanbul')

const env = process.env.NODE_ENV

module.exports = async function (karma) {
  const builds = (await import('./rollup.config.js')).default

  const build = builds[0]
  const buildPlugins = [...build.plugins]

  if (env === 'test') {
    build.plugins.push(istanbul({ exclude: ['node_modules/**/*.js', 'package.json'] }))
  }

  karma.set({
    browsers: ['chrome', 'firefox'],
    frameworks: ['jasmine'],
    reporters: ['spec', 'kjhtml'],
    logLevel: karma.LOG_WARN,

    files: [
      { pattern: './test/fixtures/**/*.js', included: false },
      { pattern: './test/fixtures/**/*.png', included: false },
      'node_modules/chart.js/dist/chart.umd.js',
      'test/index.js',
      { pattern: 'src/index.ts', type: 'js' },
      { pattern: 'test/specs/**/*.js', type: 'js' },
    ],

    customLaunchers: {
      chrome: {
        base: 'Chrome',
        flags: [
          '--disable-background-timer-throttling',
          '--disable-backgrounding-occluded-windows',
          '--disable-renderer-backgrounding',
        ],
      },
      firefox: {
        base: 'Firefox',
        prefs: {
          'layers.acceleration.disabled': true,
        },
      },
    },

    preprocessors: {
      'test/fixtures/**/*.js': ['fixtures'],
      'test/specs/**/*.js': ['rollup'],
      'test/index.js': ['rollup'],
      'src/index.ts': ['sources'],
    },

    rollupPreprocessor: {
      plugins: buildPlugins,
      external: ['chart.js'],
      output: {
        format: 'umd',
        sourcemap: karma.autoWatch ? 'inline' : false,
        globals: {
          'chart.js': 'Chart',
        },
      },
    },

    customPreprocessors: {
      fixtures: {
        base: 'rollup',
        options: {
          output: {
            format: 'iife',
            name: 'fixture',
          },
        },
      },
      sources: {
        base: 'rollup',
        options: build,
      },
    },
  })

  if (env === 'test') {
    karma.reporters.push('coverage')
    karma.coverageReporter = {
      dir: 'coverage/',
      reporters: [
        { type: 'html', subdir: 'html' },
        { type: 'lcovonly', subdir: (browser) => browser.toLowerCase().split(/[ /-]/)[0] },
      ],
    }
  }
}
