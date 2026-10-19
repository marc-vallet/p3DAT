
function getData() {
  return ENV.generateData().toArray();
}

function getTimeout() {
  return ENV.timeout;
}

function pingRenderRate() {
  Monitoring.renderRate.ping();
}

