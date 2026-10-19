
//----------------------------------

// newTimer :: Int -> IO (Promise ())
function newTimer(millis) {
  return new Promise((resolve, reject) => {
    setTimeout(
      () => {
          resolve(null);
        }
      , millis
      );
  });
}

