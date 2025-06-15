//Q1
export function all<T>(promises: Array<Promise<T>>): Promise<T[]> {
  return new Promise<T[]>((resolve, reject) => {
    const n = promises.length;
    const results: T[] = new Array(n);
    let remaining = n;

    if (n === 0) {
      resolve([]);
      return;
    }

    for (let i = 0; i < n; i++) {
      promises[i]           
        .then(value => {
          results[i] = value;  
          if (--remaining === 0) 
            resolve(results); 
        })
        .catch(reject);
    }
  });
}
  
// Q2
export function* Fib1() {
	// @TODO
  return undefined;
}


export function* Fib2() {
	// @TODO
  return undefined;
}
