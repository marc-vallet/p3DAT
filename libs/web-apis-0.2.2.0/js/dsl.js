
function _unsafeReadProtoTagged(name, value) {
  if (typeof window !== "undefined") {
    var ty = window[name];
    if (ty != null && value instanceof ty) {
      return value;
    }
  }
  var obj = value;
  while (obj != null) {
    var proto = Object.getPrototypeOf(obj);
    var constructorName = proto.constructor.name;
    if (constructorName === name) {
      return value;
    } else if (constructorName === "Object") {
      return null;
    }
    obj = proto;
  }
  return null;
}

