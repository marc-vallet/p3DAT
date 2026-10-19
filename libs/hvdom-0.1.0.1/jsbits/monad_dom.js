
function getNodeType(node) {
  return node.nodeType === 1 ? node.tagName : node.nodeType;
}

function js_set_property(propName, propValue, element) {
  if (element[propName] !== propValue) {
    element[propName] = propValue;
  }
}

function js_insert_child_ix(index, newNode, parentNode) {
  var n = parentNode.childNodes.item(index) || null;
  if (n !== newNode) {
    parentNode.insertBefore(newNode, n);
  }
}

function js_get_window() {
  return window;
}

function js_get_document(window) {
  return window.document;
}

function js_ready_state(element) {
  return element.readyState;
}

function js_crypto_random_uuid() {
  return window.crypto.randomUUID();
}

function js_unsafe_ref_eq(a, b) {
  return (a === b);
}

function js_current_target(e) {
  return e.target;
}
