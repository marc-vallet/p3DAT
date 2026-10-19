const Web_Random = {
  cryptoValue: () => {
    const array = new Uint32Array(1);
    return crypto.getRandomValues(array)[0];
  },

  splitmix32: (a1) => {
    var a2 = a1 | 0;
    var a3 = a2 + 0x9e3779b9 | 0;
    var t1 = a3 ^ (a3 >>> 15);
    var t2 = Math.imul(t1, 0x85ebca6b);
    var t3 = t2 ^ (t2 >>> 13);
    var t4 = Math.imul(t3, 0xc2b2ae35);
    var t5 = (t4 ^ (t4 >>> 16)) | 0;
    return { fst: t5, snd: a3 };
  }
}; // End of Web_Random

