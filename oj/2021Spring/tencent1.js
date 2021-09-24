/*
123@qq.com
0123456@qq.com
123456@wrong.com
a123456@qq.com

*/
const checkQQEmail = {
  init(param) {
    const that = this;
    const input = param.input;
    const output = param.output;
    console.log(input);
    if (!input || !output) return;
    // TODO: 请为input绑定blur事件，回调函数为_blur
    input.addEventListener('blur', _blur);

    function _blur() {
      console.log('in the _blur()');
      // TODO: 请获取input当前的值
      const content = input.value;
      // console.log(content);
      const emails = that.getItems(content);
      // TODO: 请筛选出错误的emails中错误的QQ邮箱
      const error = emails.filter(that.isQQEmail);
      // console.log(error);
      const map = {}; // wrong input
      error.forEach((item) => (map[item] = true));

      const right = emails.filter((item, index) => {
        // TODO: 请判断item是否重复出现过
        if (map[item] || emails.indexOf(item) !== index) {
          return false;
        }
        // TODO: 请填写正确的返回值，替换false
        return true;
      });
      // console.log(right);

      // TODO: 请将数组right通过 \n 拼接，然后赋值给input节点
      input.value = right.join('\n');
      // TODO: ��将数组error通过 \n 拼接，然后赋值给output节点
      output.value = error.join('\n');
    }
  },
  getItems(content) {
    // TODO: 请根据题目中给定的分隔符，将content分割成字符串数组，请去掉数组中的空白字符项
    const arr = content.split(/[\s,，\n]/).filter((e) => e !== '');

    // console.log(arr);
    return arr;
  },
  isQQEmail(email) {
    // TODO: 请判断email是否是正确的QQ邮箱

    const re = /^[1-9]\d{3,10}@qq.com$/;
    const res = re.test(email);
    return !res;

    // const re = /\[1-9]{4,11}@qq.com$/;
    // const test1 = re.test('12323@qq.com');
    // console.assert(test1 === true);

    // const test3 = re.test('1a323@qq.com');
    // console.assert(test3 === false);

    // const test2 = re.test('012323@qq.com');
    // console.assert(test2 === false);

    // const test5 = re.test('123@qq.com');
    // console.assert(test5 === false);

    // const test6 = re.test('1234567891011@qq.com');
    // console.assert(test6 === false);
  },
};

checkQQEmail.init({
  // TODO: 请获取class=input的节点
  input: document.querySelector('.input'),
  // TODO: 请获取class=output的节点
  output: document.querySelector('.output'),
});

const re = /^[1-9]\d{3,10}@qq.com$/;

const test7 = re.test('12345678901@qq.com'); //11 bit
console.assert(test7 === true, '7');

const test1 = re.test('12323@qq.com');
console.assert(test1 === true, '1');

const test3 = re.test('1a323@qq.com');
console.assert(test3 === false, '3');

const test2 = re.test('012323@qq.com');
console.assert(test2 === false, '2');

const test5 = re.test('123@qq.com');
console.assert(test5 === false, '5');

const test6 = re.test('1234567891011@qq.com');
console.assert(test6 === false, '6');
