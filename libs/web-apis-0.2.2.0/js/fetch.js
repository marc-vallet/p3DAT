const Web_Fetch = {
 newRequest: (url, options) => {
  try {
    return new Request(url, options);
  } catch (e) {
    console.error(e);
    throw e;
  }
 },

 bind_promise: (p,cb1,cb2) => {
  p.then( cb1
        , (err) => cb2(err.toString())
        );
 },

 typeOf: (x) => {
  if (x === null || x === undefined)
    return 0;
  if (typeof x === "number")
    return 1;
  if (typeof x === "string")
    return 2;
  if (typeof x === "boolean")
    return 3;
  if (Array.isArray(x))
    return 4;
  return 5;
 }
}; // End of Web_Fetch

