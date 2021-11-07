//@ts-nocheck
console.log(solution('rd?e?wg??'));
console.log(solution('?'));

function solution(riddle) {
  let arr = ['x', 'y', 'z'];
  for (let i = 0; i < riddle.length; i++) {
    if (riddle[i] === '?') {
      const before = riddle[i - 1];
      const after = riddle[i + 1];
      const remain = arr.filter((e) => e !== before && e !== after)[0];
      riddle = riddle.replace('?', remain);
    }
  }
  return riddle;
}

// function solution(str) {
//   const resArr = [];
//   const startArr = str.split('');

//   startArr.forEach((e, i) => {
//     const after = startArr[i + 1];
//     const before = startArr[i - 1];

//     if (e === '?') {
//       if (i === 0) {
//         resArr.push(
//           after === undefined || after === '?' ? 'a' : getDifferentChar(after)
//         );
//       }
//     }else{
// 			getDifferentChar
// 			resArr.push()
// 		}
//   });
//   return resArr.join('');
// }

// function getDifferentChar(str) {
//   const curCode = str.charCodeAt(0);
//   if (curCode === 'z'.charCodeAt(0)) return 'a';
//   return String.fromCharCode(curCode + 1);
// }
