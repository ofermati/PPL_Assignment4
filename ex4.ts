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
  let prev = 1;
  yield prev;

  let curr = 1;
  yield curr;

  while(true){
    let next = prev + curr;
    prev = curr;
    curr = next;
    yield curr;
  }
}


export function* Fib2() {
  const sqrt5 = Math.sqrt(5);
  const phi   = (1 + sqrt5) / 2;
  const psi   = (1 - sqrt5) / 2;   
  let n = 1;                      
  while (true) {
    const curr = Math.round((phi ** n - psi ** n) / sqrt5);
    yield curr;   
    n++;          
  }
}

