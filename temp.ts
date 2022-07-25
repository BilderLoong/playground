// window.downloadFile = function (sUrl) {
//   //iOS devices do not support downloading. We have to inform user about this.
//   if (/(iP)/g.test(navigator.userAgent)) {
//     alert(
//       'Your device does not support files downloading. Please try again in desktop browser.'
//     );
//     return false;
//   }

//   //If in Chrome or Safari - download via virtual link click
//   if (window.downloadFile.isChrome || window.downloadFile.isSafari) {
//     //Creating new link node.
//     var link = document.createElement('a');
//     link.href = sUrl;

//     if (link.download !== undefined) {
//       //Set HTML5 download attribute. This will prevent file from opening if supported.
//       var fileName = sUrl.substring(sUrl.lastIndexOf('/') + 1, sUrl.length);
//       link.download = fileName;
//     }

//     //Dispatching click event.
//     if (document.createEvent) {
//       var e = document.createEvent('MouseEvents');
//       e.initEvent('click', true, true);
//       link.dispatchEvent(e);
//       return true;
//     }
//   }

//   // Force file download (whether supported by server).
//   if (sUrl.indexOf('?') === -1) {
//     sUrl += '?download';
//   }

//   window.open(sUrl, '_self');
//   return true;
// };

// window.downloadFile.isChrome =
//   navigator.userAgent.toLowerCase().indexOf('chrome') > -1;
// window.downloadFile.isSafari =
//   navigator.userAgent.toLowerCase().indexOf('safari') > -1;

// [
//   ...document
//     .querySelectorAll('.emoji_card_content > table')[0]
//     .querySelectorAll('img'),
// ]
//   .map((e) => e.currentSrc)
//   .forEach((url) => {
//     downloadFile(url);
//   });

const http = require('https');
const fs = require('fs');
const path = require('path');
const urls = JSON.parse(
  '["https://www.emojiall.com/img/platform/douyin/clo.png","https://www.emojiall.com/img/platform/douyin/clp.png","https://www.emojiall.com/img/platform/douyin/clq.png","https://www.emojiall.com/img/platform/douyin/clr.png","https://www.emojiall.com/img/platform/douyin/cls.png","https://www.emojiall.com/img/platform/douyin/clt.png","https://www.emojiall.com/img/platform/douyin/clu.png","https://www.emojiall.com/img/platform/douyin/clv.png","https://www.emojiall.com/img/platform/douyin/clw.png","https://www.emojiall.com/img/platform/douyin/clx.png","https://www.emojiall.com/img/platform/douyin/cly.png","https://www.emojiall.com/img/platform/douyin/clz.png","https://www.emojiall.com/img/platform/douyin/cm_.png","https://www.emojiall.com/img/platform/douyin/cm0.png","https://www.emojiall.com/img/platform/douyin/cm1.png","https://www.emojiall.com/img/platform/douyin/cm2.png","https://www.emojiall.com/img/platform/douyin/cm3.png","https://www.emojiall.com/img/platform/douyin/cm4.png","https://www.emojiall.com/img/platform/douyin/cm5.png","https://www.emojiall.com/img/platform/douyin/cm6.png","https://www.emojiall.com/img/platform/douyin/cm7.png","https://www.emojiall.com/img/platform/douyin/cm8.png","https://www.emojiall.com/img/platform/douyin/cm9.png","https://www.emojiall.com/img/platform/douyin/cma.png","https://www.emojiall.com/img/platform/douyin/cmb.png","https://www.emojiall.com/img/platform/douyin/cmc.png","https://www.emojiall.com/img/platform/douyin/cmd.png","https://www.emojiall.com/img/platform/douyin/cme.png","https://www.emojiall.com/img/platform/douyin/cmf.png","https://www.emojiall.com/img/platform/douyin/cmg.png","https://www.emojiall.com/img/platform/douyin/cmh.png","https://www.emojiall.com/img/platform/douyin/cmi.png","https://www.emojiall.com/img/platform/douyin/cmj.png","https://www.emojiall.com/img/platform/douyin/cmk.png","https://www.emojiall.com/img/platform/douyin/cml.png","https://www.emojiall.com/img/platform/douyin/cmm.png","https://www.emojiall.com/img/platform/douyin/cmn.png","https://www.emojiall.com/img/platform/douyin/cmo.png","https://www.emojiall.com/img/platform/douyin/cmp.png","https://www.emojiall.com/img/platform/douyin/cmq.png","https://www.emojiall.com/img/platform/douyin/cmr.png","https://www.emojiall.com/img/platform/douyin/cms.png","https://www.emojiall.com/img/platform/douyin/cmt.png","https://www.emojiall.com/img/platform/douyin/cmu.png","https://www.emojiall.com/img/platform/douyin/cmv.png","https://www.emojiall.com/img/platform/douyin/cmw.png","https://www.emojiall.com/img/platform/douyin/cmx.png","https://www.emojiall.com/img/platform/douyin/cmy.png","https://www.emojiall.com/img/platform/douyin/cmz.png","https://www.emojiall.com/img/platform/douyin/cn_.png","https://www.emojiall.com/img/platform/douyin/cn0.png","https://www.emojiall.com/img/platform/douyin/cn1.png","https://www.emojiall.com/img/platform/douyin/cn2.png","https://www.emojiall.com/img/platform/douyin/cn3.png","https://www.emojiall.com/img/platform/douyin/cn4.png","https://www.emojiall.com/img/platform/douyin/cn5.png","https://www.emojiall.com/img/platform/douyin/cn6.png","https://www.emojiall.com/img/platform/douyin/cn7.png","https://www.emojiall.com/img/platform/douyin/cn8.png","https://www.emojiall.com/img/platform/douyin/cn9.png","https://www.emojiall.com/img/platform/douyin/cna.png","https://www.emojiall.com/img/platform/douyin/cnb.png","https://www.emojiall.com/img/platform/douyin/cnc.png","https://www.emojiall.com/img/platform/douyin/cnd.png","https://www.emojiall.com/img/platform/douyin/cne.png","https://www.emojiall.com/img/platform/douyin/cnf.png","https://www.emojiall.com/img/platform/douyin/cng.png","https://www.emojiall.com/img/platform/douyin/cnh.png","https://www.emojiall.com/img/platform/douyin/cni.png","https://www.emojiall.com/img/platform/douyin/cnj.png","https://www.emojiall.com/img/platform/douyin/cnk.png","https://www.emojiall.com/img/platform/douyin/cnl.png","https://www.emojiall.com/img/platform/douyin/cnm.png","https://www.emojiall.com/img/platform/douyin/cnn.png","https://www.emojiall.com/img/platform/douyin/cno.png","https://www.emojiall.com/img/platform/douyin/cnp.png","https://www.emojiall.com/img/platform/douyin/cnq.png","https://www.emojiall.com/img/platform/douyin/cnr.png","https://www.emojiall.com/img/platform/douyin/cns.png","https://www.emojiall.com/img/platform/douyin/cnt.png","https://www.emojiall.com/img/platform/douyin/cnu.png","https://www.emojiall.com/img/platform/douyin/cnv.png","https://www.emojiall.com/img/platform/douyin/cnw.png","https://www.emojiall.com/img/platform/douyin/cnx.png","https://www.emojiall.com/img/platform/douyin/cny.png","https://www.emojiall.com/img/platform/douyin/cnz.png","https://www.emojiall.com/img/platform/douyin/co_.png","https://www.emojiall.com/img/platform/douyin/co0.png","https://www.emojiall.com/img/platform/douyin/co1.png","https://www.emojiall.com/img/platform/douyin/co2.png","https://www.emojiall.com/img/platform/douyin/co3.png","https://www.emojiall.com/img/platform/douyin/co4.png","https://www.emojiall.com/img/platform/douyin/co5.png","https://www.emojiall.com/img/platform/douyin/co6.png","https://www.emojiall.com/img/platform/douyin/co7.png","https://www.emojiall.com/img/platform/douyin/co8.png","https://www.emojiall.com/img/platform/douyin/co9.png","https://www.emojiall.com/img/platform/douyin/coa.png","https://www.emojiall.com/img/platform/douyin/cob.png","https://www.emojiall.com/img/platform/douyin/coc.png","https://www.emojiall.com/img/platform/douyin/cod.png","https://www.emojiall.com/img/platform/douyin/coe.png","https://www.emojiall.com/img/platform/douyin/cof.png","https://www.emojiall.com/img/platform/douyin/cog.png","https://www.emojiall.com/img/platform/douyin/coh.png","https://www.emojiall.com/img/platform/douyin/coi.png","https://www.emojiall.com/img/platform/douyin/coj.png","https://www.emojiall.com/img/platform/douyin/cok.png","https://www.emojiall.com/img/platform/douyin/com.png","https://www.emojiall.com/img/platform/douyin/coo.png","https://www.emojiall.com/img/platform/douyin/cop.png","https://www.emojiall.com/img/platform/douyin/coq.png","https://www.emojiall.com/img/platform/douyin/cor.png","https://www.emojiall.com/img/platform/douyin/cos.png","https://www.emojiall.com/img/platform/douyin/cot.png","https://www.emojiall.com/img/platform/douyin/cou.png","https://www.emojiall.com/img/platform/douyin/cov.png","https://www.emojiall.com/img/platform/douyin/cow.png","https://www.emojiall.com/img/platform/douyin/cox.png","https://www.emojiall.com/img/platform/douyin/coy.png","https://www.emojiall.com/img/platform/douyin/coz.png","https://www.emojiall.com/img/platform/douyin/cp_.png","https://www.emojiall.com/img/platform/douyin/cp0.png","https://www.emojiall.com/img/platform/douyin/cp1.png","https://www.emojiall.com/img/platform/douyin/cp2.png","https://www.emojiall.com/img/platform/douyin/cp3.png","https://www.emojiall.com/img/platform/douyin/cp4.png","https://www.emojiall.com/img/platform/douyin/cp5.png","https://www.emojiall.com/img/platform/douyin/cp6.png","https://www.emojiall.com/img/platform/douyin/cp7.png","https://www.emojiall.com/img/platform/douyin/cp8.png","https://www.emojiall.com/img/platform/douyin/cp9.png","https://www.emojiall.com/img/platform/douyin/cpa.png","https://www.emojiall.com/img/platform/douyin/cpb.png","https://www.emojiall.com/img/platform/douyin/cpc.png","https://www.emojiall.com/img/platform/douyin/cpd.png","https://www.emojiall.com/img/platform/douyin/cpe.png","https://www.emojiall.com/img/platform/douyin/cpf.png","https://www.emojiall.com/img/platform/douyin/cpg.png","https://www.emojiall.com/img/platform/douyin/cph.png","https://www.emojiall.com/img/platform/douyin/cpi.png"]'
);

function downloadFile(url, dest) {
  const file = fs.createWriteStream(dest);

  http.get(url, (res) => {
    res.pipe(file);
    file.on('finish', () => {
      file.close();
    });
  });
}

urls.forEach((url) => {
  const name = url.split('/').pop();
  // downloadFile(url, path.join(__dirname, 'temp', name));
  console.log(name);
});
// console.log(url);

type keys = '123' | 1 | 2;
type Foo = {
  [key in keys]: key;
};
//-->
/*
type Foo = {
 index: "stringKey" | 1 | 2;
}
*/
let foo: Foo = {
  123: 1,
  1: 1,
  2: 2,
};
=======
export {};
import { Readable } from 'stream';
import { createInterface } from 'readline';

// The string used to debug, don't forget add \n to trigger the 'line' event;
const DEBUG_STRING = 'asdfasd asd fasd f asd\n';
let s;

if (DEBUG_STRING) {
  s = new Readable();
  s.push(DEBUG_STRING);
  s.push(null);
}

// If provide the debug string, prior to use it.
const inputStream = s || process.stdin;
const rl = createInterface({
  input: inputStream,
  output: process.stdout,
});

// `lines` is used to store all the lines from input stream;
// Each line is stored as an entry of the array.
// const lines: string[] = [];
rl.on('line', (line: string) => {
  const arr: number[][] = [];
  let res = 0;

  arr.push(line.split(' ').map((e) => parseInt(e)));
  if (arr.length > 2) {
    const [row, col] = arr[0];
    const [x, y] = arr[1];

    arr.slice(2).forEach((curLine, curRowNum) => {
      curLine.forEach((e, curColNum) => {
        res += e;
      });
    });

    res = res - arr[x - 1][y - 1] + 1;
  }

  console.log(res);
});
