import simulate from 'miniprogram-simulate';
import path from 'path';

it('should ', () => {
  const id = simulate.load(path.join(__dirname, '../comp/index')); // 加载自定义组件，返回组件 id
  console.log(id);
  const comp = simulate.render(id); // 使用 id 渲染自定义组件，返回组件封装实例

  const parent = document.createElement('parent-wrapper'); // 创建容器节点
  comp.attach(parent); // 将组件插入到容器节点中，会触发 attached 生命周期

  if (comp.dom) {
    expect(comp.dom.innerHTML).toBe('<div>123</div>'); // 判断组件渲染结果
    // 执行其他的一些测试逻辑

    comp.detach(); // 将组件从容器节点中移除，会触发 detached 生命周期
  }
});
