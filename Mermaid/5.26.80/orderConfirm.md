``` mermaid
gra$$

$$ph TD
    A(进入菜单页) --> B{URL 中 authGeo 参数为 1}
    B --> |Y| G[获取地理位置信息]
    G --> Z1[loadFMP 和 pageSPU 请求接口中增加经纬度信息] --> P{ Are there missing shopId, tableNum, bizMoe in the URL parameters ** }
  P --> |Y| Store[ Store the shopId, tableNum, bizMode into URL, state, storage ]
    Store -->  I{根据接口响应判断是否跳转选择门店页} & S{根据接口响应判断是否限制用户点餐}
    I --> |是| T1(跳转选择门店页)
    S --> |是| L1("隐藏操作区按钮，并显示状态栏提示用户")
    S --> |否| END(继续正常菜单页流程)
    I --> |否| END(继续正常菜单页流程)
    B --> |否| END
```
