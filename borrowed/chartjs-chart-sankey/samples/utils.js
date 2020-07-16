'use strict'
;(function (Utils) {
  const chartjsUrl = 'https://cdn.jsdelivr.net/npm/chart.js@3.2.1/dist/chart.js'
  const localUrl = '../dist/chartjs-chart-sankey.js'
  const remoteUrl = 'https://cdn.jsdelivr.net/npm/chartjs-chart-sankey/dist/chartjs-chart-sankey.js'

  function addScript(url, done, error) {
    const head = document.getElementsByTagName('head')[0]
    const script = document.createElement('script')
    script.type = 'text/javascript'
    script.onreadystatechange = function () {
      if (this.readyState === 'complete') {
        done()
      }
    }
    script.onload = done
    script.onerror = error
    script.src = url
    head.appendChild(script)
    return true
  }

  function loadError() {
    const msg = document.createTextNode('Error loading chartjs-chart-sankey')
    document.body.appendChild(msg)
    return true
  }

  Utils.load = function (done) {
    addScript(
      chartjsUrl,
      () => {
        addScript(localUrl, done, (event) => {
          event.preventDefault()
          event.stopPropagation()
          addScript(remoteUrl, done, loadError)
        })
      },
      loadError
    )
  }
})((window.Utils = window.Utils || {}))
