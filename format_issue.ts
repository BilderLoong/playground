/* eslint-disable @typescript-eslint/naming-convention */
import Immutable from "seamless-immutable";
import rmsStorage from "@api/rmsStorage";
import Toast from "@main/lib/mix/toast";
import Limo from "@limo/core";
import { advertiseReporter } from "@mtfe/rms-sdk";
import {
  AD_TYPE,
  BIZ_TYPE_MAP,
  BOSS_DISH_SHOW_TYPE,
  DEFAULT_ERROR_TITLE,
  DISH_SHARE_TYPE,
  DISH_SHOW_TYPE,
  DISH_SOURCE,
  DISH_TYPE,
  MENU_PAGE_INIT_CONFIG,
  ORDER_BIZ_TAG,
  PORTAL_TAB_TYPE,
  SHARE_INFO_SOURCE,
  TITLE_CONTENT,
} from "@constants/bizConstants";
import { OP_NAME, OP_TYPE } from "@constants/lxConstants";
import {
  DISH_ITEM_TYPE_MAP,
  DISH_ITEM_TYPE_VAL,
  PANEL_TYPE_VAL,
  POINT_DISH_TYPE,
} from "@constants/menu";
import { getTabBarUrl, isTakeAwayOrSelfPick } from "@main/lib/mix/util";
import { RAPTOR_PERFORMANCE } from "@constants/reportConstants";
import { HIDE_CART_BAR_WHEN_EMPTY } from "@constants/carBarConstants";
import {
  appendExposeDishItem,
  exposeDishItemNew,
} from "@modules/menu/dish/expose";
import {
  categoryListHasIcon,
  generateCategoryList,
  getPluginActivityToCategory,
} from "@modules/menu/category/CategoryDataTransfer";
import {
  getCompleteParameters,
  getLocationInfo,
  handleTapTipsBannerButton,
  hasEnoughOptions,
  saveLocationInfo,
  showTakeCouponTipsHandle,
} from "@modules/menuHelper";
import { judgeComplexSpuDish, judgeFixedPackage } from "@modules/cartHelper";
import {
  toggleContainerPanelAction,
  updateCalculator,
  updateCurrentDishId,
  updateEditSkuDishAction,
} from "@store/actions/baseCart";
import { transToSkuDish } from "@modules/operateCartHelper";
import {
  setExtraInfo,
  setMpUserInfo,
  setUrlParams,
} from "@store/actions/extraInfo";
import { setAddOnShowAction } from "@store/actions/add-on";
import {
  addDish,
  addToCart,
  editWeightSkuDish,
  minusCartGrouponDish,
  minusDish,
  openDishDetail,
  setDishAddFromAction,
  showCalcPanel,
  toggleRegisterPanelAction,
  updateBaseInfo,
} from "@store/actions/cart";
import { PointPurchase } from "@store/cart/PointPurchaseService";
import Cart from "@modules/menu/cart/cartSdk";
import { setDishList } from "@store/actions/dish";
import {
  showDishTimeInfoModal,
  showTakeCouponPanelAction,
  toggleDishInfoModal,
  updatePrePriceTips,
} from "@store/actions/panel";
import {
  setAddressListModalFlag,
  setUserAddressList,
  updateTakeawayConfigInfo,
} from "@store/actions/takeaway";
import {
  getUserAddressList,
  selectAddress,
  updateCloudCacheCartData,
  updatePickUpDistance,
} from "@store/asyncActions/takeaway";
import {
  getErrPageInfo,
  switchBiz,
  viewOtherShop,
  getTakeawayUrlParams,
} from "@modules/takeaway/takeawayHelper";
import { getParamsFromCookie } from "@modules/takeaway/util";
import { getNewApp, setCloudDataCacheKey } from "@main/lib/wx/app-info";
import { setNavigationBarTitle } from "@main/lib/wx/util";
import { sendMC, sendPV, sendMV } from "@modules/LXHelper";
import { EXPOSE_DISH_TYPE } from "@modules/menu/dish/expose";
import Triangle from "@mtfe/rms-triangle-c";
import { initPageOnLoad, ProxyPage } from "@main/lib/wx/page";
import { plugins } from "@lib/wx/plugin/diancan";
import {
  generateDishList,
  getSetObj,
  sliceDataTransfer,
} from "@modules/menu/dish/DishDataTransfer";
import {
  dealOperationData,
  dealPreventSale,
} from "@modules/menu/dish/PropertyUtil";
import { getUserInfo } from "@main/lib/userInfo";
import { CONTAINER_NAME } from "@main/constants/decorate";
import shopApi from "@api/shop";
import {
  clearLastShopInfo,
  getAllSpuWithCache,
  getMainData,
  initUser,
  saveCardInfo,
  showMemberLoginToast,
  showMustDishToast,
} from "@modules/menu/pageViewModel";
import Log from "@main/lib/mix/log";
import { dealOperationCountStyle } from "@modules/menu/dish/DishDataUtil";
import { MpPageViewModel } from "@modules/menu/limo/MpPageViewModel";
import { DishDetailPointParams } from "@type/menu/Menu";
import { ErrorTips, ResponseState } from "@type/IResponse";
import { MenuMainData } from "@type/newMenu/UIModel";
import { transformRpxToNumber } from "@lib/wx/transfer-rpx";
import { transaction } from "@mtfe/rms-sdk";
import { FAIL_REASON } from "@modules/menu/dataSource/point";
import GrouponCouponSdk from "@modules/menu/grouponCoupon/GrouponCouponSdk";
// import { toErrorTip } from '@lib/navigator';
import { ERROR_TYPE } from "@constants/errorTip";
import { checkEnv } from "@modules/error-tip/errorTipHelper";
import { getCurrentPagePath, customReLaunch } from "@lib/navigator";

import { composePage } from "@store/helpers/compose";
import {
  FIXED_HEAD_POSITION_MP,
  setAnimation,
} from "@modules/menu/scrollAnimation/index";
import { menuLoadFailPoint, setFmpMenuListSuccess } from "./utils/point";
import {
  addDataUpdateListener,
  sendElseMenuListRenderDuration,
  sendInitPageToRequestStart,
  sendLVC,
  sendMenuListRenderDuration,
  sendRequestEndToDataStart,
  sendRequestStartToRequestEnd,
  sendSplashMenuFmpDuration,
  updateIsFirstEnter,
} from "./utils/perfLog";
import { PANEL_TYPE, PanelTransaction } from "@modules/panel/panelTransaction";
import { URLParams } from "@type/common/URLParameters";
import { getTabbarPageOptions } from "@main/lib/wx/util";
import { isNativeTabbarPage, setTabbarPageParams } from "@main/lib/wx/app-info";
import { setPointPurchase } from "@store/actions/point-purchase";
import { WEI_QIAN_TANENT_ID } from "@lib/wx/weiqian-special";
import { ICON_TYPE } from "@constants/decorConstants";
import MustDishSdk from "@modules/MustDishSdk";
import { TriggeredEvents as AdvertisingModalTriggeredEvents } from "../../../../components/limo-menu/advertising-modal/protocol";
const isAliPayNative = process.env.PLATFORM === "alipay";
const SEARCH_BANNER_SHOW = 1;
const SEARCH_BANNER_HIDE = 0;

const pageConfig = {
  pageViewModel: MpPageViewModel,
  // TrianglePage会调用, 一般页面只有一个首屏模块的不需要做额外定义
  defineFmpCondition: () => ({
    conditions: ["shopInfo", "recycleList"],
  }),
  // Limo,
  data: {
    showFullLoading: true,
    dishShowType: "",
    allDishList: [], // 切片后的菜单列表数据
    finishSetDishList: false,
    firstScreenCollectEnd: false,
    reachBoundary: "",
    menuScrollY: true,
    categoriesList: [], // 左侧菜品分类列表
    isMember: false,
    options: {},
    showOrderedDetailPanel: false, // 点过浮层状态
    expandedShopInfo: false, // 商家信息是否展开，展开是不展示购物车
    resultCode: 100,
    recycleWinWidth: 0,
    isShowBigBtn: true, // 菜单页展示添加菜品大按钮
    hasTabBar: false,
    initErrData: {},
    errorText: "",
    menuUpdateTime: 0,
    categoryStickyTop: FIXED_HEAD_POSITION_MP,
    hasEnoughOptions: false,
    location: {},
    isNativeTabbarPage: false,
    errorInfo: null,
    hideCartBarWhenEmpty: HIDE_CART_BAR_WHEN_EMPTY.menu,
  },
  isFirstShowToast: true, // 团购券使用张数限制，提示次数是否为第一次
  realCategoryCount: 0, //全量菜品的分类数统计，用来和首屏作对比
  isFirstSet: true, //是不是第一次触发列表渲染时间上报
  renderStartTime: 0, //列表开始渲染时间
  takeCouponData: {}, // 不参与渲染，避免浪费性能
  isDestroy: false,
  shareMsg: {},
  urlParams: {},
  isFirstEnterPage: true, // 是否第一次进入页面，进入其他页面之后再返回为false,不需要上报性能以及秒开率
  //时段菜信息弹窗组件参数
  dishTimeInfoModalRequestInfo: {
    requestHost: "",
    requestParams: {},
  },
  menuDetailScrollHeight: 0,
  async handleLoad(options: URLParams.MenuPage) {
    if (options && Object.keys(options).length) {
      await this._initPageShow(options);
    }
  },
  onPageScroll(info) {
    const scrollTop = info?.scrollTop;

    // 设置菜单列表是否到达边界
    if (
      scrollTop < this.menuDetailScrollHeight &&
      this.data.reachBoundary !== "top"
    ) {
      this.setData({
        reachBoundary: "top",
      });
    } else if (
      scrollTop >= this.menuDetailScrollHeight &&
      this.data.reachBoundary
    ) {
      this.setData({
        reachBoundary: "",
      });
    }
    //页面触顶校准
    if (scrollTop < 5 && this.data.reachBoundary !== "top") {
      this.setData({ reachBoundary: "top" });
      Limo &&
        Limo.setModuleData("menu-shop-info", {
          searchBannerOpacity: SEARCH_BANNER_HIDE,
        });
    }

    // 滚动过程中设置吸顶容器的sticky位置
    if (
      scrollTop >= this.stickyContainerScrollHeight &&
      !this.stickyContainerFixed
    ) {
      this.stickyContainerFixed = true;
      Limo && Limo.setModuleData("top-sticky-container", { fixed: true });
    } else if (
      scrollTop < this.stickyContainerScrollHeight &&
      this.stickyContainerFixed
    ) {
      this.stickyContainerFixed = false;
      Limo && Limo.setModuleData("top-sticky-container", { fixed: false });
    }
    if (!this.data.firstScreenCollectEnd) {
      this.setData({ firstScreenCollectEnd: true });
    }
    // 页面菜品滚动
    // 设置吸顶搜索 / 分类
    setAnimation(scrollTop, this, Limo);
  },
  onReachBottom() {
    this.setData({
      reachBoundary: "bottom",
    });
    Limo &&
      Limo.setModuleData("menu-shop-info", {
        searchBannerOpacity: SEARCH_BANNER_SHOW,
      });
  },
  switchCategory(event) {
    //锚定到正确分类
    const windowWidth = Triangle.MPInfo.getSystemInfo().windowWidth || 375;
    const categoryId = event.detail;
    const query = this.createSelectorQuery();

    query.selectViewport().scrollOffset();
    query.select(`.container >>> #item-${categoryId}`).boundingClientRect();
    query.exec((res) => {
      const nowScrollTop: number = (res[0] && res[0].scrollTop) || 0;
      const nowTabTop: number = (res[1] && res[1].top) || 0; // 菜品列表中当前item距离视口顶部的高度
      // 菜单分类在左侧
      const isDefaultCategory =
        this.data.dishShowType === DISH_ITEM_TYPE_VAL.DEFAULT ||
        this.data.dishShowType === DISH_ITEM_TYPE_VAL.RIGHT_LARGE;
      const headerHeight: number =
        // eslint-disable-next-line @typescript-eslint/restrict-plus-operands
        this.data.categoryStickyTop +
        ((isDefaultCategory ? 0 : 36) / 375) * windowWidth;
      wx.pageScrollTo({
        scrollTop: nowScrollTop + nowTabTop - headerHeight,
        duration: 0,
      });
    });
    //头部透明度
    Limo &&
      Limo.setModuleData("menu-shop-info", {
        searchBannerOpacity: SEARCH_BANNER_SHOW,
      });
  },
  //获取tabBar选中的type
  getSelectedPageType() {
    const options = this.urlParams;
    let selectedPageType;

    if (isTakeAwayOrSelfPick(options.bizType)) {
      selectedPageType =
        Number(options.bizType) === BIZ_TYPE_MAP.TAKEAWAY
          ? PORTAL_TAB_TYPE.TAKEAWAY
          : PORTAL_TAB_TYPE.PICKUP;
    } else {
      selectedPageType =
        Number(options.reserveMode) === DISH_SOURCE.RESERVE
          ? PORTAL_TAB_TYPE.PRE_ORDER
          : PORTAL_TAB_TYPE.ORDER_MENU;
    }
    return selectedPageType;
  },
  loadTabBar(options: URLParams.MenuPage) {
    let catchTabBarList = rmsStorage.getTabBarData(
      options.restaurantViewId || options.shopId,
    );
    const previewFlag = getNewApp().diancanGlobalData.getPreview(
      CONTAINER_NAME.TAB_BAR,
    );
    const requestParams = {
      params: {
        restaurantViewId: options.restaurantViewId || "",
        mtShopId: options.shopId || "",
        entrance: options.entrance || "",
        tenantId: options.tenantId || "",
        previewFlag,
        minaId: options.minaId || "",
        portalUrl: options.portalUrl || "",
        wmChannel: getParamsFromCookie("orderChannel") || -1,
      },
      url: getTabBarUrl(),
    };
    const selectedPageType = this.getSelectedPageType();

    if (previewFlag) {
      catchTabBarList = [];
      rmsStorage.setTabBarData(options.restaurantViewId || options.shopId, []);
    }
    this.setData({
      requestParams: catchTabBarList?.length ? {} : requestParams,
      catchTabBarList,
      selectedPageType,
    });
    return {
      isVirtualTabbar: !!catchTabBarList?.length,
    };
  },
  async onLoad(options: URLParams.MenuPage | { qrCode: string }) {
    initPageOnLoad();
    Limo.proxy({
      limoUseCustomHandler: {
        sendMC,
        sendPV,
        sendMV,
        Toast,
        Log,
        Triangle,
        ThemeUtil: {
          ICON_TYPE,
        },
        rmsStorage,
        appendExposeDishItem,
        exposeDishItemNew,
        dealOperationData,
        Cart,
        MustDishSdk,
        dealOperationCountStyle,
        advertiseReporter,
      },
    });
    options = getTabbarPageOptions(options, this.route);

    if (!this.optionsGuard(options)) {
      return;
    }

    const newOptions = {
      ...options,
      multiShop: options.multiShop || rmsStorage.getMultiShop(),
    };
    this.setData({
      options: newOptions,
      // Checking whether url parameter complete.
      hasEnoughOptions: hasEnoughOptions(newOptions),
      extraGetDecorationRequestParam: { shopCache: rmsStorage.getShopCache() },
      isNativeTabbarPage: !!isNativeTabbarPage(this.route),
    });
    updateIsFirstEnter(this.isFirstEnterPage);
    addDataUpdateListener(this);
    // 记录页面加载时间点，用于计算首屏时间
    this.pageLoadTime = Date.now();

    this.options = options;
    showMustDishToast(options);
    //从门店列表页长链接进入外卖菜单页场景。本地缓存下cacheKey
    this._updateCacheKey(options.cacheKey);

    Limo.registerEvent(
      "openAdSpuDetailEvent",
      (e: AdvertisingModalTriggeredEvents["openAdSpuDetailEvent"]) => {
        this.openAdvertiseSpuDetail(e);
      },
    );
    // 会员小黑条登录成功回调
    Limo.registerEvent("universalLogin-uniLoginSuccess", async () => {
      await this.handleLoad(this.urlParams);
    });

    await this.handleLoad(options);
  },

  checkWeiQian(options) {
    const { tenantId } = options;

    if (Number(tenantId) === WEI_QIAN_TANENT_ID) {
      const query = Triangle.stringify(options);
      rmsStorage.setMenuOps(query);
    }
  },
  /**
   * @description Update complete url parameter.
   * @param options complete url parameter object.
   */
  saveUrlParameters(options: URLParams.MenuPage) {
    // Checking whether url parameter complete.
    this.setData({ options, hasEnoughOptions: hasEnoughOptions(options) });
    // 味千门店需要缓存菜单页入参
    this.checkWeiQian(options);
    this.urlParams = options;
    this.setUrlParams(options);
    // Update options in case some components using getMixUrlParam() to get options information.
    this.options = options;
  },

  /**
   @description Check whether url parameter is correct.
   * @returns true - options is eligible to continue the following process.
   */
  optionsGuard(options) {
    if (this.checkOldPerception(options)) {
      // 虽然内部有跳转，还是阻止掉往下走的异常流程
      return false;
    }

    // If `options` only container `qrCode` property.
    if (
      isAliPayNative &&
      Object.keys(options).length === 1 &&
      "qrCode" in options
    ) {
      const app = getApp();
      app.qrCode = options.qrCode;

      this.loadFail(FAIL_REASON.MALFORMED_OPTIONS);

      wx.reLaunch({ url: "/pages/splash/index" });

      return false;
    }

    return true;
  },

  checkOldPerception(options) {
    // 路由逻辑已经统一到splash页面，兜底处理下可能的残留流量，参数中有 p 或者 q的跳走
    const { p, q } = options;
    if (p || q) {
      const query = Triangle.stringify(options, false);
      this.loadFail(FAIL_REASON.OLD_PERCEPTION);
      Triangle.redirectTo({ url: `/pages/splash/index?${query}` });
      return true;
    }
    return false;
  },

  async onShow() {
    sendPV("c_saaspay_gzi8cqe9");
    this.showTakeCouponTips = false;
    this.saveWMCloudData = false;

    if (!this.isFirstEnterPage) {
      this.setData({
        errorInfo: null,
      });
      let options = Object.assign(this.options, {});
      options = getTabbarPageOptions({}, this.route, options);
      this.saveUrlParameters(options);
      setTabbarPageParams(this.route, options);
      if (this.checkOldPerception(options)) return;
      this._updateCacheKey(this.options.cacheKey);
      await this.handleLoad(this.urlParams);
    }

    // 页面onShow时重新设置菜单固定头部透明度，menu-shop-info property中默认为0，频繁在菜单页和会员页间切换可能导致该值被重置，导致固定头部消失
    if (this.searchBannerOpacity) {
      Limo &&
        Limo.setModuleData("menu-shop-info", {
          searchBannerOpacity: this.searchBannerOpacity,
        });
    }
  },

  onReady() {
    const app = getNewApp();
    try {
      if (!app.isHide) {
        const coldStartDuration = Date.now() - app.report.launchTimeFromScan;
        Log.addPerformance(RAPTOR_PERFORMANCE.MP.APP_READY, coldStartDuration); // 小程序launch到页面首次渲染耗时，冷启动耗时减去这个耗时可以计算出包加载耗时
      }
    } catch (e) {}
    this.updateMpUserInfo();
  },

  updateComponentsScrollTop() {
    const query = this.createSelectorQuery();
    let categoryStickyTop = this.data.categoryStickyTop;
    let fixedHeadHeight = FIXED_HEAD_POSITION_MP;
    // 菜单固定头部
    query.select(`.container >>> .common-head-fixed`).boundingClientRect();
    // 时间条
    query.select(`.container >>> .top-sticky-container`).boundingClientRect();
    // 菜单列表
    query.select(`.container >>> .menu-detail`).boundingClientRect();
    // 页面滚动高度
    query.selectViewport().scrollOffset();
    query.exec((res) => {
      if (!Array.isArray(res)) return;
      const [
        commonHeadFixed = {},
        topStickyContainer,
        menuDetail = {},
        viewportScroll,
      ] = res;
      const viewportScrollTop = viewportScroll.scrollTop || 0; // 页面已滚高度
      // res[0]菜单固定头部
      if (commonHeadFixed && commonHeadFixed.height) {
        fixedHeadHeight = commonHeadFixed.height;
        this.setData({ categoryStickyTop: fixedHeadHeight });
      }
      // res[1]是时间条
      const { top, height } = topStickyContainer || {};
      if (top && height) {
        // 已经吸顶则不更新滚动高度
        if (top !== fixedHeadHeight) {
          this.stickyContainerScrollHeight =
            viewportScrollTop + top - fixedHeadHeight;
        }
        // 设置菜单分类的sticky位置
        categoryStickyTop = fixedHeadHeight + topStickyContainer.height;
        this.setData({ categoryStickyTop });
        // 设置吸顶容器的 sticky 位置
        Limo.setModuleData("top-sticky-container", {
          offsetTop: fixedHeadHeight,
        });
      }
      // res[2]是菜单列表的位置
      if (menuDetail && menuDetail.top) {
        this.menuDetailScrollHeight =
          viewportScrollTop + menuDetail.top - categoryStickyTop;
      }
    });
  },

  // 上报首屏时间、扫码耗时
  sendFstScanDuration() {
    const app = getNewApp();
    if (this.isFirstEnterPage) {
      // 菜单页onLoad-FMP
      this.loadSuccess("recycleList");
    }
    const { perceptionPage } = this.urlParams;
    if (perceptionPage) {
      // 新的FST
      sendSplashMenuFmpDuration(app.report?.splashLoadTime); // 新的fmp定义=splash的onLoad到menu的fmp
      app.report.splashLoadTime = -1;
    }
  },

  // 初始化 Limo 数据
  async initLimoData(limoCompsData, dealCoupons) {
    this.pageViewModel = new MpPageViewModel({
      pageInstance: this,
      Limo: Limo.getLimoRuntime(),
      options: this.urlParams,
    });
    const { shopId } = this.urlParams;
    // 初始化团购券数据，需要在第一次算价前完成
    await GrouponCouponSdk.initDealCoupons(
      shopId,
      this.pageViewModel,
      dealCoupons,
    );
    return this.pageViewModel.getMenuViewModel(limoCompsData);
  },

  _updateCacheKey(cacheKey: string) {
    cacheKey && setCloudDataCacheKey(cacheKey);
  },

  checkPointPurchase(fmpBizData) {
    if (fmpBizData?.pointBuyCampaign?.campaignAndSkuRelations) {
      this.setPointPurchase(
        fmpBizData.pointBuyCampaign.campaignAndSkuRelations,
      );
    }
  },

  async _initPageShow(options: URLParams.MenuPage) {
    const initPageShowStart = Date.now();
    const _requestMenuDataTime = sendInitPageToRequestStart(initPageShowStart);

    const { bizType, authGeo, reserveMode } = options;

    let location;
    // 自提，外卖和点餐叫号取餐开了距离判断的情况下需要获取用户的本地地址，或者 authGeo === '1'.
    if (
      isTakeAwayOrSelfPick(bizType) ||
      authGeo === URLParams.AuthGeo.REQUIRED
    ) {
      await transaction("MENU.getLocation", async () => {
        location = await getLocationInfo({
          canUseSessionCache: true,
          defaultTimeout: 500000,
          showAuthSettingDialog: false,
        });

        if (location) {
          this.setData({
            location,
          });
        }
      });
    }

    // 获取menulist数据
    const res: MenuMainData | ResponseState.REDIRECT = await getMainData({
      ...options,
      ...(location ? { userGeoPoint: location } : {}),
    });

    if (res === ResponseState.REDIRECT) {
      // Triangle.hideLoading();
      // Report redirect page status to avoid unexpected
      // timeout page status (sended when onUnload defined in triangle page.).
      this.loadFail(FAIL_REASON.REDIRECT);
      return;
    }

    const {
      pageTitle, // 标题
      memberInfo,
      localHeadInfo,
      spuDetail, // 非菜品数据,为了方便limo敏捷，这里的部分模型需要转换成旧的模型
      errorTips,
      spuPromise,
      moduleData,
      moduleSortList,
      userInfo,
      orderProductionProcessVO,
    } = res;
    const requestEnd = sendRequestStartToRequestEnd(_requestMenuDataTime);

    const { shopConfig, shopInfo, fmpBizData } = localHeadInfo ?? {};
    const {
      recSceneFlag,
      shopCache,
      showCouponPackage = false,
    } = fmpBizData ?? {};
    // Complete/update url parameters after got response data.
    const newOptions = getCompleteParameters({
      shopConfig,
      fmpBizData,
      oldParameter: options,
    });

    this.checkPointPurchase(fmpBizData);

    // Will assign newOption to this.urlParams.
    this.saveUrlParameters(newOptions);
    setTabbarPageParams(this.route, newOptions);

    if (this.isDestroy) {
      // 页面redirectTo到其他页面，已经销毁了，即调用了onUnLoad,防内存泄露
      return;
    }

    if (errorTips) {
      this.dealErrorInfo(errorTips);
      return;
    }

    if (!localHeadInfo) {
      this.dealErrorInfo({
        errorType: FAIL_REASON.EMPTY_HEAD,
        errorMsg: "localHeadInfo为空",
      });
      return;
    }

    // Query tab data after getting fmp data.
    const { isVirtualTabbar = false } = this.loadTabBar(newOptions);
    const { shopId, tableNum, spuId, cacheKey } = this.urlParams;
    this._updateCacheKey(cacheKey);

    this.localHeadInfo = localHeadInfo;
    // 设置页面标题栏商户名称
    // 返回场景下没有loading态阻止交互，该代码执行时可能用户已经去了其他页面
    if (isAliPayNative && my.canIUse("hideBackHome")) {
      // 支付宝小程序隐藏home
      my.hideBackHome();
    } else {
      setNavigationBarTitle({ title: pageTitle }, "/menu/");
    }
    // Sava location info to cloud store for order confirm web page to use in pick bizMode.
    saveLocationInfo({ bizType, cacheKey, location: this.data.location });

    Log.setAsyncOptions("shopId", shopId);
    clearLastShopInfo(newOptions);
    this.setExtraInfo(shopId, localHeadInfo);
    this.updateBaseInfo(shopId, tableNum);
    showMemberLoginToast(memberInfo);
    saveCardInfo(shopId, memberInfo);
    // Update `recSceneFlag` when it isn't `null`.
    if (recSceneFlag !== null) {
      rmsStorage.setRecSceneFlag(Boolean(recSceneFlag));
    }

    if (shopCache) {
      rmsStorage.setShopCache(shopCache);
    }

    const { menuStyle, spuMatchRecommendPages, mtShopId } = shopConfig || {};
    const { dishShowType = 0, spuComboShowType = 0 } = menuStyle || {};
    const horizontal =
      dishShowType > 0 && dishShowType !== DISH_SHOW_TYPE.RIGHT_LARGE;
    let column = 1;

    if (dishShowType === DISH_SHOW_TYPE.TWO_IN_ROW) {
      column = 2;
    } else if (dishShowType === DISH_SHOW_TYPE.THREEE_IN_ROW) {
      column = 3;
    }

    const { groupCouponInfo, shopMarketings } = shopInfo || {};
    const { showMTBind, dealCoupons, mtUserName } = groupCouponInfo || {};
    const setDataStart = sendRequestEndToDataStart(requestEnd);
    const limoMenuData = await this.initLimoData(
      { moduleData, moduleSortList, progressInfo: orderProductionProcessVO },
      dealCoupons,
    );

    //登录后提示是否领取成功
    if (this.showTakeCouponTips && moduleData) {
      showTakeCouponTipsHandle(moduleData, memberInfo, () => {
        this.showTakeCouponTips = false;
      });
    }

    const waiMaiConfig = moduleData?.["menu-shop-info"]?.data?.waiMaiConfig;
    if (waiMaiConfig) {
      this.updateTakeawayConfigInfo(waiMaiConfig.takeAwayDeliveryFeeRule);
    }

    const renderData = {
      limoMenuData,
      userInfo,
      memberInfo, // 门店标签、公告、logo、名称、展开后、会员 limo
      actionUrl: memberInfo?.actionUrl,
      pageTitle,
      horizontal,
      showMTBind,
      mtUserName,
      spuMatchRecommendPages,
      column,
      recycleWinWidth: transformRpxToNumber(
        MENU_PAGE_INIT_CONFIG[
          `INIT_${DISH_ITEM_TYPE_MAP[dishShowType]}_RECYCLE_VIEW_WIDTH`
        ],
      ),
      spuComboShowType,
      menuUpdateTime: Date.now(),
      currentShopId: mtShopId, // 通过缓存拿到的门店数据，链接上无shopId
      // 商家小程序  && 扫码点餐/自提 才支持装修展示开关
      showCouponPackage:
        menuStyle?.feConfig?.showPayCouponPackage === "false"
          ? false
          : showCouponPackage,
      payCouponDecoInfo: {
        // 付费券包装修数据
        couponPageSize:
          menuStyle?.feConfig?.couponPageSize &&
          Number(menuStyle?.feConfig?.couponPageSize),
        payCouponPackageShowImgUrl:
          menuStyle?.feConfig?.payCouponPackageShowImgUrl,
        sizeScale: menuStyle?.feConfig?.couponPageSizeScale,
      },
      shopMarketings,
    };
    transaction(
      "MENU.SET_DATA.SHOP_INFO",
      async (t) => {
        this.setData(renderData, () => {
          t.success();
          // 除了菜品长列表的菜单页信息
          if (this.isFirstEnterPage) {
            this.loadSuccess("shopInfo");
          }
          sendElseMenuListRenderDuration(setDataStart); // 非长列表渲染耗时，即setData耗时
          if (Triangle.isByteDanceMicroApp) {
            /**
             * TODO: by qiankeyu
             * 抖音中在setData回调中，无法通过createSelectorQuery直接取到limo-container中的元素，setTimeout一段时间后可以取到。
             * 先单独适配抖音小程序，后续再看是否能与微信保持一致。
             */
            setTimeout(() => {
              this.updateComponentsScrollTop();
            }, 200);
          } else {
            // 微信、支付宝走原逻辑
            this.updateComponentsScrollTop(); // 设置sticky组件的滚动高度
          }
          Limo && Limo.setModuleData("menu-shop-info", { isVirtualTabbar });
        });
      },
      true,
    );
    this.checkNewAdvanceToast();
    // 页面参数中包含spuId, 代表需要popup详情弹窗
    this.popUpDetailPanel(spuId);
    if (this.isFirstEnterPage) {
      this.dealRenderList(spuDetail, spuPromise);
    } else {
      this.updateMenuList(spuPromise);
    }
  },

  dealRenderList(spuMap, spuPromise) {
    const isFMP = !!spuPromise;
    const { categories, shopConfig, memberInfo, shopInfo, fmpBizData } =
      this.localHeadInfo;
    const menuStyle = shopConfig?.menuStyle;
    const isLogin = !!memberInfo?.cardId;
    const {
      recommendShowType = BOSS_DISH_SHOW_TYPE.DEFAULT,
      dishShowType = 0,
      menuType = 0,
    } = menuStyle || {};
    const shopMarketings = shopInfo?.shopMarketings || [];
    const { pointBuyCampaign } = fmpBizData;

    //”插件“数据整合到分类里
    const newCategories = getPluginActivityToCategory(
      categories,
      shopMarketings,
      spuMap,
    );

    const localCategories = generateCategoryList(isFMP, {
      categories: newCategories,
      dishShowType,
      list: spuMap,
      cartList: this.data.cartDishList,
      isLogin,
    });
    const categoryHasIcon = categoryListHasIcon(localCategories);
    // 菜品数据填充
    const dishShowTypeVal =
      DISH_ITEM_TYPE_MAP[dishShowType] || DISH_ITEM_TYPE_VAL.DEFAULT;

    const dishList = generateDishList(isFMP, {
      categories: localCategories,
      list: spuMap,
      dishShowType: dishShowTypeVal,
      cartList: this.data.cartDishList,
      menuStyle,
      pointBuyCampaign,
    });
    // 刷新分类列表
    this.setData({
      menuType,
      categoriesList: localCategories,
      categoryHasIcon: isFMP
        ? categories.some((category) => !!category.categoryMultimediaList)
        : categoryHasIcon,
      dishShowType: dishShowTypeVal,
      recommendShowType,
    });
    //记录下全量菜品的分类数，用来和首屏作比较
    if (!spuPromise) {
      this.realCategoryCount = dishList?.length || 0;
    }
    this.renderDishList(dishList, spuPromise);
  },

  // 渲染菜单长列表
  renderDishList(dishList, spuPromise) {
    this.renderStartTime = this.isFirstEnterPage ? Date.now() : 0; // 参考sendPerfLog
    // setContent 临时数据 放了循环 setData
    const setContent = sliceDataTransfer(dishList);
    // console.log('setContent====', setContent);
    // const start_time = Date.now();
    let count = 0;
    setContent.forEach((item) => {
      this.setData(item, () => {
        count++;
        if (this.isFirstSet) {
          // 上报首屏时间、扫码耗时
          this.sendFstScanDuration();
          // 首屏列表渲染耗时
          sendMenuListRenderDuration(this.renderStartTime);
          // 首屏菜品渲染成功
          setFmpMenuListSuccess();
          this.isFirstSet = false;
        }
        if (count === setContent.length) {
          // [setData打点]初始化 dishList 的 setData 完成
          // console.log('allDishList====', this.properties.allDishList, Date.now() - start_time);
          this.updateMenuList(spuPromise);
          this.removeExtraCategories();
        }
      });
    });
  },
  //删除首屏可能多余的分类，融合项目后可删除该段逻辑
  removeExtraCategories() {
    if (
      this.realCategoryCount > 0 &&
      this.data.allDishList.length > this.realCategoryCount
    ) {
      for (
        let i = this.realCategoryCount;
        i < this.data.allDishList?.length;
        i++
      ) {
        this.setData({
          [`allDishList[${i}]`]: null,
        });
      }
    }
  },

  async updateMenuList(spuPromise) {
    if (!spuPromise) {
      // All pageSpuInfo data is loaded.
      // Update the `menuUpdateTime` trigger the `recommendation-module` uses newest dish data.
      Limo &&
        Limo.setModuleData("recommendation-module", {
          menuUpdateTime: Date.now(),
        });
      // 关闭页面loading
      // Triangle.hideLoading();
      if (this.isFirstEnterPage) {
        sendLVC(this.pageLoadTime);
      }
      this.setData({
        showFullLoading: false,
        finishSetDishList: true,
      });

      return;
    }
    const { shopId, tenantId, mandatoryPrompt } = this.urlParams;
    const { shopConfig, shopInfo } = this.localHeadInfo;

    let allSpu;
    try {
      allSpu = await getAllSpuWithCache(this.urlParams, spuPromise);
    } catch (e) {
      Log.addError("全量菜品网络异常", e);
    }
    if (!allSpu) {
      this.dealErrorInfo({
        errorType: FAIL_REASON.ALL_DISH_FAIL,
        errorMsg: DEFAULT_ERROR_TITLE,
      });

      return;
    }

    const { mandatoryInfos, cartConfig } = shopConfig || {};
    this.setDishList(shopId, allSpu);
    rmsStorage.setUseMenuCacheForNonMenuPage(shopId, true); // 搜索页可以使用全量菜作为缓存
    this.initNewCart(
      mandatoryInfos,
      allSpu,
      cartConfig?.openTogether,
      decodeURIComponent(mandatoryPrompt ?? ""),
    );
    this.dealRenderList(allSpu, null); // 刷新完整菜单后，promise传null,不用再刷新了
    GrouponCouponSdk.showMTAccountToast(
      shopInfo,
      rmsStorage.getGrouponCouponList(shopId),
    );
    this.shopHomeShare(shopId, tenantId);
    initUser();
  },

  dealErrorInfo(e: ErrorTips) {
    // Triangle.hideLoading();
    const { errorType, errorMsg } = e || {};
    const pageType = this.getSelectedPageType();
    menuLoadFailPoint({ errorType, errorMsg });
    this.loadFail(errorType);
    //https://p0.meituan.net/travelcube/10ad2114d0910ffe72dc8c2afd7574b037900.png
    const errPageInfo = getErrPageInfo(errorType);
    let type =
      errorType === ERROR_TYPE.NEED_DOWNGRADE_CODE
        ? errorType
        : ERROR_TYPE.NEED_RELOAD;
    let errUrl = "";
    let btnText =
      errorType === ERROR_TYPE.NEED_DOWNGRADE_CODE ? "去点餐" : "重新加载";
    if (errPageInfo.type) {
      type = errPageInfo.type;
      errUrl = errPageInfo.url;
      btnText = errPageInfo.btnText;
    }
    this.setData({
      errorInfo: {
        errorTitle: errorMsg,
        btnConfig: {
          buttonText: btnText,
        },
        redirectUrl: errUrl || "",
        type,
      },
      // btnText,
      // url: errUrl,
    });
    // toErrorTip({
    //   type,
    //   errorTip: errorMsg,
    //   pageType,
    //   url: errUrl,
    //   btnText,
    // });
  },

  handleErrorClick() {
    const { errorInfo } = this.data;
    const { type, redirectUrl } = errorInfo || {};
    switch (+type) {
      case ERROR_TYPE.NEED_DOWNGRADE_CODE: {
        const pagePath = getCurrentPagePath();
        this.openIndependentApplets(pagePath);
        return;
      }
      case ERROR_TYPE.CUSTOM_JUMP: {
        Triangle.redirectTo({
          url: redirectUrl,
        });
        return;
      }
      case ERROR_TYPE.NEED_RELOAD:
      default: {
        const params = getTakeawayUrlParams();
        const { multiShop } = params || {};
        customReLaunch("menu", {
          multiShop,
        });
        return;
      }
    }
  },

  openIndependentApplets(targetPage) {
    // wx1fde2c33280d64b6 线上
    // wx06a469c9bcc9f1ea 线下
    const appId =
      checkEnv() === "develop" ? "wx06a469c9bcc9f1ea" : "wx1fde2c33280d64b6";
    wx.navigateToMiniProgram({
      appId,
      path: targetPage,
      extraData: {},
      envVersion: checkEnv(), // 环境设置，联调时跳开发版
      success: (res) => {
        console.log("小程序打开成功", res);
      },
      fail: (error) => {
        console.log("小程序打开失败", error);
        Log.addError(`小程序打开失败`, error);
      },
    });
  },

  checkNewAdvanceToast() {
    const { orderBizTag, hasOrder } = this.urlParams;
    if (+orderBizTag === ORDER_BIZ_TAG.AHEAD && hasOrder === "true") {
      Triangle.showToast({
        icon: "none",
        title: "该桌台有订单，请您联系服务员或更换桌台用餐",
        duration: 3000,
      });
    }
  },

  async shopHomeShare(shopId, tenantId) {
    const options = this.urlParams;
    const bizType = Number(options.bizType);
    let sharePageSource;
    if (bizType === BIZ_TYPE_MAP.PICKUP || bizType === BIZ_TYPE_MAP.TAKEAWAY) {
      sharePageSource = bizType;
    } else {
      sharePageSource = Number(options.reserveMode)
        ? SHARE_INFO_SOURCE.PRE_ORDER
        : SHARE_INFO_SOURCE.ORDER;
    }
    const params = {
      containerName: "shop-share-config",
      previewFlag: false,
      bizId: shopId,
      bizIdType: "10",
      tenantId,
      sharePageSource,
    };
    try {
      const shareConfig = await shopApi.getShopHomeShare(params);
      const { shareMsg, shareSwitch } = shareConfig;
      if (shareSwitch) {
        this.shareMsg = shareMsg;
      } else {
        wx.hideShareMenu({ menus: ["shareAppMessage"] });
      }
    } catch (error) {
      console.log(error);
    }
  },

  onShareAppMessage(e) {
    const { species } = e?.target?.dataset || {};
    if (e?.from === "button" && species === DISH_SHARE_TYPE)
      return this.dishShareInfo;
    return this.shareMsg;
  },

  getSpuById(spuId) {
    const mtShopId = this.urlParams.shopId;
    if (!spuId || !mtShopId) {
      return null;
    }
    const dishList = rmsStorage.getDishList(mtShopId);
    if (!dishList) {
      return null;
    }
    return dishList[spuId];
  },

  // 将 app 上的头像昵称信息更新到页面上
  updateMpUserInfo() {
    const { nickname, headimgurl } = getUserInfo();
    const currMpUserInfo = { nickname, headimgurl };
    this.setMpUserInfo(currMpUserInfo);
  },
  popUpDetailPanel(spuId) {
    const spu = this.getSpuById(spuId);
    // 菜品禁售
    // 或参数存在spuId, 但未找到菜品
    const itemSoldOut = dealPreventSale(spu) || (!spu && spuId);
    const { alreadyPopup } = this.data;
    if (!alreadyPopup && spuId) {
      if (itemSoldOut) {
        Triangle.showToast({
          icon: "none",
          title: "菜品已售完, 看看其他菜品吧~",
          duration: 3000,
        });
      } else {
        this.menuDetail({
          detail: spu,
        });
      }

      this.setData({
        alreadyPopup: true,
      });
    }
  },
  // 打开菜品详情 - 老板推荐打开详情
  menuDetail(event) {
    const dish = event.detail;
    if (!dish) return;
    const spuId = dish.spuId;
    const { shopId } = this.urlParams;
    const extraInfo = rmsStorage.getExtraInfo(shopId) || {};
    const bossRecommendText = extraInfo.shopInfo?.recommendInfos?.boss?.title;
    this.openDishDetail({
      ...dish,
      reportConfig: {
        ...(dish.reportConfig || {}),
        ...(dish.__reportConfig__ || {}),
        bossRecommendText,
        dishType: dish.dishType,
        spuName: dish.spuName,
        adFrom: EXPOSE_DISH_TYPE.BOSSRECOMMEND,
        spuId,
        skuId: dish.skuMenuItems ? dish.skuMenuItems[0]?.skuId : 0,
      },
    });
    const paramsCommon = {
      op_type: OP_TYPE.RECOMMEND_DISH, // 功能区名称
      op_name: dish.__reportConfig__?.fromNetRecommend
        ? OP_NAME.RECOMMEND_NET
        : OP_NAME.RECOMMEND_BOSS, // 功能名称
      sn: dish.__reportConfig__?.index, // 序号
      show_title: dish.__reportConfig__?.fromNetRecommend
        ? TITLE_CONTENT.NET
        : TITLE_CONTENT.BOSS, // 展示标题
      spu_id: spuId,
      skuId: dish?.skuMenuItems[0]?.skuId,
    };
    let paramsFinal: DishDetailPointParams = { ...paramsCommon };
    // 老板推荐时添加is_defined_title参数
    if (!dish.__reportConfig__?.fromNetRecommend) {
      const isDefinedTitle = bossRecommendText ? 1 : 0;
      paramsFinal = {
        is_defined_title: isDefinedTitle,
        ...paramsCommon,
        show_title: bossRecommendText,
      };
    }

    // 新增菜品详情埋点
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      clickData: {
        ...paramsFinal,
      },
    });
  },
  // 统一加菜
  addDishFuncMenu(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.addDish(skuDish);
  },
  onClickCountNum(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.showCalcPanel(skuDish);
  },
  // 统一减菜
  minusDishFuncMenu(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.minusDish(skuDish);
  },
  // 监听头部商家详情的展开情况
  expandShopInfo(e) {
    const { showShopFlag } = e.detail;
    this.setData({
      expandedShopInfo: showShopFlag,
      isShowToastFromMTLogin: false,
    });
  },
  hidePanel() {
    this.toggleRegisterPanelAction(false);
    this.toggleContainerPanelAction(false);
    this.setAddOnShow(false);
    this.closeTakeCouponPanel();
  },
  onHide() {
    this.isFirstEnterPage = false;
    updateIsFirstEnter(this.isFirstEnterPage);
    this.hidePanel();
    this.updateCloudCacheCartData();
    this.saveWMCloudData = true;
  },
  onUnload() {
    this.isDestroy = true;
    this.hidePanel();
    //onHIde生命周期执行完之后。onUnload内不用再执行一次了
    if (!this.saveWMCloudData) {
      this.updateCloudCacheCartData();
    }
    // 跳走时可能有没关的
    Triangle.hideLoading();
  },
  showPanelType(e) {
    const {
      operationData = {},
      skuId = "",
      __newReportConfig__,
      exposeDishType = "",
      menuReportConfig,
    } = e.detail;
    // 处理关联菜品逻辑
    const { panelType, id, reportConfig, discount = "" } = operationData;
    console.log("operationData==", operationData);
    if (panelType && id) {
      const containerOpenExtraData = {
        reportConfig: {
          ...(menuReportConfig || {}),
          ...reportConfig,
        },
      };
      this.updateCurrentDishId({
        currentSpuId: id,
        currentSkuId: skuId,
        containerOpenExtraData,
        __newReportConfig__,
        currentDiscount: discount,
      });
      this.toggleContainerPanelAction(true);
      this.setDishAddFromAction(exposeDishType);
      // 【Panel触发】根据PanelType打开container-panel中不同的panel，包括多规格、套餐、计算器
      switch (panelType) {
        case PANEL_TYPE_VAL.SPEC:
          PanelTransaction.init(PANEL_TYPE.MULTI_PANEL);
          break;
        case PANEL_TYPE_VAL.PACKAGE:
          PanelTransaction.init(PANEL_TYPE.PACKAGE_PANEL);
          break;
        case PANEL_TYPE_VAL.WEIGHT:
          PanelTransaction.init(PANEL_TYPE.CALCULATOR_PANEL);
          break;
        default:
          break;
      }
    }
  },
  // 菜品详情 - 菜单打开
  showDishPanel(e) {
    const spuId = e.detail.dish.spuId;
    const isFirstScreen = e.detail.isFirstScreen;
    const { dish } = e.detail;
    const spu = this.getSpuById(spuId);
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      // 菜品图片点击埋点
      tab_id: dish.categoryId,
      dishes_id: dish.spuId,
      dishes_name: dish.name,
      dishes_price: Number(dish.currentPrice),
      dishes_status: spu.soldOut ? 1 : spu.canSaleNow ? 0 : undefined,
    });
    if (!spu || !dish) {
      return;
    }
    const { shopId } = this.urlParams;
    const extraInfo = rmsStorage.getExtraInfo(shopId);
    const bossRecommendText = extraInfo?.shopInfo?.recommendInfos?.boss?.title;
    this.openDishDetail({
      ...spu,
      reportConfig: {
        ...(dish.reportConfig || {}),
        ...(dish.__reportConfig__ || {}),
        bossRecommendText,
        dishType: dish.dishType || spu.dishType,
        spuName: dish.spuName || spu.spuName,
        adFrom: EXPOSE_DISH_TYPE.DISHLIST,
        spuId,
        skuId: dish.skuMenuItems ? dish.skuMenuItems[0]?.skuId : 0,
        categoryId: dish.categoryId,
        isFirstScreen,
      },
    }); // 补充菜品详情打点需要的参数
    // 【Panel触发】菜单列表上打开菜品详情面板
    PanelTransaction.init(PANEL_TYPE.DISH_DETAIL_PANEL);
    let opName = OP_NAME.OTHER;
    let showTitle = "";
    // categoryId大于0是分类推荐
    if (dish.categoryId > 0) {
      opName = OP_NAME.RECOMMEND_CLASS;
    } else {
      opName = OP_NAME.RECOMMEND_DISH;
    }
    showTitle = dish.__reportConfig__?.belongCategory?.parentName;

    // 新增菜品详情埋点
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      clickData: {
        op_type: OP_TYPE.RECOMMEND_CLASS, // 功能区名称
        op_name: opName, // 功能名称
        show_title: showTitle, // 展示标题
        spu_id: spuId,
        sn: dish.__reportConfig__?.index, // 序号
      },
    });
  },

  addToCartCoupon(e) {
    const { dish } = e.detail;
    const { spuId, skuId, count } = dish;
    GrouponCouponSdk.autoUseCouponAddToCart(
      spuId,
      skuId,
      count,
      () => {
        this.addToCart(dish);
      },
      () => {
        this.addToCart(dish);
      },
    );
  },
  operateCartDish(e) {
    const { type, id, showCal = false, skuId = "", discount = "" } = e.detail;
    const spu = this.getSpuById(id);
    if (!spu) {
      return;
    }
    // 积分购
    const isPointPurchase = !!(discount === POINT_DISH_TYPE);

    const dish = transToSkuDish(
      spu,
      this.data.cartDishList,
      skuId,
      isPointPurchase,
    );
    if (!dish) {
      return;
    }

    if (showCal) {
      this.updateCurrentDishId({
        currentSpuId: id,
        currentDiscount: discount,
      });
      this.toggleContainerPanelAction(true);
      this.updateCalculator({
        isShowCountCalc: true,
      });
      this.updateEditSkuDishAction({
        editDishSpuId: dish.spuId,
        editDishGoodsNo: dish.goodsNo,
      });
      return;
    }
    if (type === "plus") {
      if (isPointPurchase) {
        this.pointPurchaseAdd(dish);
        return;
      }

      GrouponCouponSdk.autoUseCouponAddToCart(
        dish.spuId,
        dish.skuId,
        1,
        () => {
          this.addDish(dish);
        },
        (singleSpu) => {
          if (singleSpu.dishType === DISH_TYPE.PACKAGE) {
            // 如果团购券对应的菜品为固定套餐，无需弹出套餐面板，直接加入购物车
            if (judgeFixedPackage(singleSpu)) {
              this.addFixedPackage(singleSpu, this.data.refactorLocalCart);
            } else {
              this.updateCurrentDishId({
                currentSpuId: singleSpu.spuId,
              });
              this.toggleContainerPanelAction(true);
            }
          } else {
            if (judgeComplexSpuDish(singleSpu)) {
              this.updateCurrentDishId({
                currentSpuId: singleSpu.spuId,
              });
              this.toggleContainerPanelAction(true);
            } else {
              const singleSkuDish = Immutable.getIn(singleSpu, [
                "skuMenuItems",
                0,
              ]);
              this.addDish(singleSkuDish);
            }
          }
        },
      );
    } else if (type === "minus") {
      // if (isPointPurchase) {
      //   this.pointPurchaseMinus(dish);
      //   return;
      // }
      this.minusDish(dish);
    }
  },
  tabBarRenderFn(e) {
    const { tabBarList = [] } = e.detail;
    if (!this.data.hasTabBar && tabBarList.length) {
      Limo && Limo.setModuleData("menu-shop-info", { isVirtualTabbar: true });
      this.setData({
        hasTabBar: true,
      });
    }
    const { requestParams } = this.data;
    const { restaurantViewId = "", mtShopId = "" } = requestParams.params || {};
    if (restaurantViewId || mtShopId) {
      rmsStorage.setTabBarData(restaurantViewId || mtShopId, tabBarList);
    }
  },
  openAdvertiseSpuDetail(
    event: AdvertisingModalTriggeredEvents["openAdSpuDetailEvent"],
  ) {
    const { exposeDishType, adType, spu, reportConfig } = event;
    this.setDishAddFromAction(exposeDishType);
    if (adType === AD_TYPE.SPU) {
      if (!spu) {
        return;
      }
      this.openDishDetail({
        ...spu,
        reportConfig,
      });
    }
  },
  showDishTimeInfoModalFn(e) {
    const { spuId } = e.detail;
    this.showDishTimeInfoModal(spuId);
  },
  dishTimeInfoModalEvent() {
    this.toggleDishInfoModal(false);
  },
  gotoThirdLogin() {
    // this.showLoadingOnShow = true;
    this.pageViewModel.gotoThirdLoginClick();
  },
  useGrouponCoupon(e) {
    const { discountTempId, spuId, skuId } = e.detail || {};
    if (!discountTempId || !spuId || !skuId) return;
    const { shopId } = this.urlParams;
    const dishList = rmsStorage.getDishList(shopId);
    if (!dishList) {
      return;
    }
    const spuDish = dishList[spuId];
    const skuDish = spuDish?.skuMenuItems?.find((item) => item.skuId === skuId);
    GrouponCouponSdk.addGrouponCouponDishToCart(
      [discountTempId],
      spuDish,
      skuDish,
      false, // 手动使用团购券，非自动
      (singleSpu) => {
        if (singleSpu.dishType === DISH_TYPE.PACKAGE) {
          if (judgeFixedPackage(singleSpu)) {
            this.addFixedPackage(singleSpu, this.data.refactorLocalCart);
          } else {
            this.updateCurrentDishId({ currentSpuId: singleSpu.spuId }); // 当前选中菜品的spuId
            this.toggleContainerPanelAction(true);
          }
        } else {
          if (judgeComplexSpuDish(singleSpu)) {
            this.updateCurrentDishId({ currentSpuId: singleSpu.spuId });
            this.toggleContainerPanelAction(true);
          } else {
            const dish = Immutable.getIn(singleSpu, ["skuMenuItems", 0]);
            this.addDish(dish); // 加菜的逻辑
          }
        }
      },
    );
  },
  cancelGrouponCoupon(e) {
    const { discountTempId, spuId } = e.detail || {};
    const minusGoodsNo = GrouponCouponSdk.cancelDealCoupon(discountTempId);
    minusGoodsNo && this.minusCartGrouponDish(spuId, minusGoodsNo);
  },
  //关闭领券弹窗
  closeTakeCouponPanel() {
    this.showTakeCouponPanelAction({
      takeCouponPanelTag: false,
    });
  },
  // 弹领券浮层
  openCouponPanel(e) {
    this.showTakeCouponPanelAction({
      takeCouponPanelTag: true,
      sourceType: e.detail.from,
    });
  },
  async handleTakeCoupon() {
    this.showTakeCouponTips = true;
    await this._initPageShow(this.urlParams);
  },
  // 根据分类id更新allDishList
  updateDishList(e) {
    const { categoryId } = e.detail;
    const setObj = getSetObj(
      this.data.allDishList,
      categoryId,
      this.data.dishShowType,
    );
    this.setData(setObj);
  },
  // 根据modifyObj的key和value更新allDishList
  modifyDishList(e) {
    const { modifyObj } = e.detail;
    if (!modifyObj) return;
    this.setData(modifyObj);
  },
  selectedAddressFn(e) {
    const { address } = e.detail;
    this.selectAddress(address);
  },
  handleAddNewAddressFn() {
    const { fmpBizData = {} } = this.localHeadInfo;
    const { addressPageUrl = "" } = fmpBizData;
    if (addressPageUrl) {
      Triangle.navigateTo({ url: addressPageUrl });
    }
  },
  closeAddressListModal() {
    this.setAddressListModalFlag(false);
  },
  getUserAddressList() {
    this.getUserAddressList();
  },
  updateAddressFn(e) {
    const { addressList = [] } = e.detail;
    this.setUserAddressList(addressList);
  },
  toggleShop(e) {
    const { bizType } = e.detail;
    viewOtherShop(bizType);
  },
  async switchBiz(e) {
    const { bizTypeStatus } = e.detail;
    // 切换自提和外卖时清空购物车
    this.clearCart();
    const { fmpBizData = {} } = this.localHeadInfo;
    const { addressPageUrl = "" } = fmpBizData;
    await switchBiz(bizTypeStatus, addressPageUrl);
  },
  updatePickUpDistanceFn() {
    this.updatePickUpDistance();
  },

  async onTapTipsBannerButton(event) {
    await handleTapTipsBannerButton({
      event,
      onGotLocation: (location) => {
        if (this.urlParams.authGeo !== URLParams.AuthGeo.REQUIRED) {
          // Reason to change authGeo on the query to REQUIRED:
          //  In situations like merchant opens the order distance restriction switch when the menu page already gets the page query.
          //  The authGeo parameter on the query doesn't equal '1',
          //  but the back end needs location info to verify the order distance.
          this.saveUrlParameters({
            ...this.urlParams,
            authGeo: URLParams.AuthGeo.REQUIRED,
          });
        }

        this.setData({ location });
        // Reload page.
        // Because the getLocationInfo() will store the location information into storage
        // and use stored location information after reloading.
        // Triangle.showLoading({ title: '加载中' });
        this._initPageShow(this.urlParams);
      },
      Limo,
    });
  },

  closeOrderProgress() {
    rmsStorage.setOrderProgress(this.urlParams.shopId);
  },
  updatePrePriceTipsFn() {
    if (this.data.showPrePriceTips) {
      this.updatePrePriceTips(false);
    }
  },
};

const mapStateToData = (state) => ({
  cartDishList:
    Immutable.getIn(state, ["cart", "cartDishList"]) || Immutable({}),
  cartDishSortMapList:
    Immutable.getIn(state, ["cart", "cartDishSortMapList"]) || Immutable([]),
  spuDish: Immutable.getIn(state, ["cart", "spuDish"]),
  mpUserInfo: Immutable.getIn(state, ["extraInfo", "mpUserInfo"]),
  showContainerPanelTag: Immutable.getIn(state, [
    "cart",
    "showContainerPanelTag",
  ]),
  showDishInfoModal: Immutable.getIn(state, ["panel", "showDishInfoModal"]),
  dishTimeInfoModalRequestInfo: Immutable.getIn(state, [
    "panel",
    "dishTimeInfoModalRequestInfo",
  ]),
  refactorLocalCart: Immutable.getIn(state, ["cart", "refactorLocalCart"]),
  takeCouponPanelInfo:
    Immutable.getIn(state, ["panel", "takeCouponPanelInfo"]) || {},
  takeCouponPanelData:
    Immutable.getIn(state, ["panel", "takeCouponPanelData"]) || {},
  showPrePriceTips: Immutable.getIn(state, ["panel", "showPrePriceTips"]),
});

const mapDispatchToData = (dispatch) => ({
  // 积分购加减菜
  pointPurchaseAdd: (skuDish) => dispatch(PointPurchase.addDish(skuDish)),
  // pointPurchaseMinus: skuDish => dispatch(PointPurchase.minusDish(skuDish)),
  setPointPurchase: (campaignAndSkuRelations) =>
    dispatch(setPointPurchase(campaignAndSkuRelations)),
  addDish: (skuDish) => dispatch(addDish(skuDish)),
  addToCart: (skuDish) => dispatch(addToCart(skuDish)),
  minusDish: (skuDish) => dispatch(minusDish(skuDish)),
  minusCartGrouponDish: (spuId, goodsNo) =>
    dispatch(minusCartGrouponDish(spuId, goodsNo)),
  openDishDetail: (spuDish) => dispatch(openDishDetail(spuDish)),
  updateBaseInfo: (mtShopId, mtTableNum) =>
    dispatch(updateBaseInfo(mtShopId, mtTableNum)),
  setDishList: (mtShopId, dishList) =>
    dispatch(setDishList(mtShopId, dishList)),
  setExtraInfo: (mtShopId, headInfo) =>
    dispatch(setExtraInfo(mtShopId, headInfo)),
  // @ts-ignore: TODO: 扩张参数必须具有元祖类型或传递给rest参数
  initNewCart: (...args) => dispatch(Cart.init(...args)),
  setUrlParams: (params) => dispatch(setUrlParams(params)),
  setMpUserInfo: (params) => dispatch(setMpUserInfo(params)),
  showCalcPanel: (skuDish) => dispatch(showCalcPanel(skuDish)),
  updateCalculator: (calcInfo) => dispatch(updateCalculator(calcInfo)),
  editWeightSkuDish: (skuDish) => dispatch(editWeightSkuDish(skuDish)),
  // 多规格操作时--
  toggleRegisterPanelAction: (showRegisterPanel) =>
    dispatch(toggleRegisterPanelAction(showRegisterPanel)),
  updateCurrentDishId: (dishId) => dispatch(updateCurrentDishId(dishId)),
  toggleContainerPanelAction: (show) =>
    dispatch(toggleContainerPanelAction(show)),
  setDishAddFromAction: (dishAddFrom) =>
    dispatch(setDishAddFromAction(dishAddFrom)),
  updateEditSkuDishAction: (editSkuDishInfo) =>
    dispatch(updateEditSkuDishAction(editSkuDishInfo)),
  toggleDishInfoModal: (showDishInfoModal) =>
    dispatch(toggleDishInfoModal(showDishInfoModal)),
  showDishTimeInfoModal: (spuId) => dispatch(showDishTimeInfoModal(spuId)),
  showTakeCouponPanelAction: (takeCouponInfo) =>
    dispatch(showTakeCouponPanelAction(takeCouponInfo)),
  addFixedPackage: (menuSpu, refactorLocalCart) =>
    dispatch(Cart.addFixedPackage(menuSpu, refactorLocalCart)),
  setAddOnShow: (isShow) => dispatch(setAddOnShowAction(isShow)),
  selectAddress: (addressId) => dispatch(selectAddress(addressId)),
  getUserAddressList: () => dispatch(getUserAddressList()),
  setUserAddressList: (addressList) =>
    dispatch(setUserAddressList(addressList)),
  setAddressListModalFlag: (tag) => dispatch(setAddressListModalFlag(tag)),
  updateTakeawayConfigInfo: (takeAwayDeliveryFeeRule) =>
    dispatch(updateTakeawayConfigInfo(takeAwayDeliveryFeeRule)),
  clearCart: () => {
    dispatch(Cart.clearCart());
    sendMC("b_saaspay_lxmzp99s_mc");
  },
  updatePickUpDistance: () => dispatch(updatePickUpDistance()),
  updateCloudCacheCartData: () => dispatch(updateCloudCacheCartData()),
  updatePrePriceTips: (tag) => dispatch(updatePrePriceTips(tag)),
});

const nextPageConfig = composePage(
  mapStateToData,
  mapDispatchToData,
  pageConfig,
);

ProxyPage(nextPageConfig, plugins);
import Immutable from "seamless-immutable";
import rmsStorage from "@api/rmsStorage";
import Toast from "@main/lib/mix/toast";
import Limo from "@limo/core";
import { advertiseReporter } from "@mtfe/rms-sdk";
import {
  AD_TYPE,
  BIZ_TYPE_MAP,
  BOSS_DISH_SHOW_TYPE,
  DEFAULT_ERROR_TITLE,
  DISH_SHARE_TYPE,
  DISH_SHOW_TYPE,
  DISH_SOURCE,
  DISH_TYPE,
  MENU_PAGE_INIT_CONFIG,
  ORDER_BIZ_TAG,
  PORTAL_TAB_TYPE,
  SHARE_INFO_SOURCE,
  TITLE_CONTENT,
} from "@constants/bizConstants";
import { OP_NAME, OP_TYPE } from "@constants/lxConstants";
import {
  DISH_ITEM_TYPE_MAP,
  DISH_ITEM_TYPE_VAL,
  PANEL_TYPE_VAL,
  POINT_DISH_TYPE,
} from "@constants/menu";
import { getTabBarUrl, isTakeAwayOrSelfPick } from "@main/lib/mix/util";
import { RAPTOR_PERFORMANCE } from "@constants/reportConstants";
import { HIDE_CART_BAR_WHEN_EMPTY } from "@constants/carBarConstants";
import {
  appendExposeDishItem,
  exposeDishItemNew,
} from "@modules/menu/dish/expose";
import {
  categoryListHasIcon,
  generateCategoryList,
  getPluginActivityToCategory,
} from "@modules/menu/category/CategoryDataTransfer";
import {
  getCompleteParameters,
  getLocationInfo,
  handleTapTipsBannerButton,
  hasEnoughOptions,
  saveLocationInfo,
  showTakeCouponTipsHandle,
} from "@modules/menuHelper";
import { judgeComplexSpuDish, judgeFixedPackage } from "@modules/cartHelper";

import {
  toggleContainerPanelAction,
  updateCalculator,
  updateCurrentDishId,
  updateEditSkuDishAction,
} from "@store/actions/baseCart";
import { transToSkuDish } from "@modules/operateCartHelper";
import {
  setExtraInfo,
  setMpUserInfo,
  setUrlParams,
} from "@store/actions/extraInfo";
import { setAddOnShowAction } from "@store/actions/add-on";
import {
  addDish,
  addToCart,
  editWeightSkuDish,
  minusCartGrouponDish,
  minusDish,
  openDishDetail,
  setDishAddFromAction,
  showCalcPanel,
  toggleRegisterPanelAction,
  updateBaseInfo,
} from "@store/actions/cart";
import { PointPurchase } from "@store/cart/PointPurchaseService";
import Cart from "@modules/menu/cart/cartSdk";
import { setDishList } from "@store/actions/dish";
import {
  showDishTimeInfoModal,
  showTakeCouponPanelAction,
  toggleDishInfoModal,
  updatePrePriceTips,
} from "@store/actions/panel";
import {
  setAddressListModalFlag,
  setUserAddressList,
  updateTakeawayConfigInfo,
} from "@store/actions/takeaway";
import {
  getUserAddressList,
  selectAddress,
  updateCloudCacheCartData,
  updatePickUpDistance,
} from "@store/asyncActions/takeaway";
import {
  getErrPageInfo,
  switchBiz,
  viewOtherShop,
  getTakeawayUrlParams,
} from "@modules/takeaway/takeawayHelper";
import { getParamsFromCookie } from "@modules/takeaway/util";
import { getNewApp, setCloudDataCacheKey } from "@main/lib/wx/app-info";
import { setNavigationBarTitle } from "@main/lib/wx/util";
import { sendMC, sendPV, sendMV } from "@modules/LXHelper";
import { EXPOSE_DISH_TYPE } from "@modules/menu/dish/expose";
import Triangle from "@mtfe/rms-triangle-c";
import { initPageOnLoad, ProxyPage } from "@main/lib/wx/page";
import { plugins } from "@lib/wx/plugin/diancan";
import {
  generateDishList,
  getSetObj,
  sliceDataTransfer,
} from "@modules/menu/dish/DishDataTransfer";
import {
  dealOperationData,
  dealPreventSale,
} from "@modules/menu/dish/PropertyUtil";
import { getUserInfo } from "@main/lib/userInfo";
import { CONTAINER_NAME } from "@main/constants/decorate";
import shopApi from "@api/shop";
import {
  clearLastShopInfo,
  getAllSpuWithCache,
  getMainData,
  initUser,
  saveCardInfo,
  showMemberLoginToast,
  showMustDishToast,
} from "@modules/menu/pageViewModel";
import Log from "@main/lib/mix/log";
import { dealOperationCountStyle } from "@modules/menu/dish/DishDataUtil";
import { MpPageViewModel } from "@modules/menu/limo/MpPageViewModel";
import { DishDetailPointParams } from "@type/menu/Menu";
import { ErrorTips, ResponseState } from "@type/IResponse";
import { MenuMainData } from "@type/newMenu/UIModel";
import { transformRpxToNumber } from "@lib/wx/transfer-rpx";
import { transaction } from "@mtfe/rms-sdk";
import { FAIL_REASON } from "@modules/menu/dataSource/point";
import GrouponCouponSdk from "@modules/menu/grouponCoupon/GrouponCouponSdk";
// import { toErrorTip } from '@lib/navigator';
import { ERROR_TYPE } from "@constants/errorTip";
import { checkEnv } from "@modules/error-tip/errorTipHelper";
import { getCurrentPagePath, customReLaunch } from "@lib/navigator";

import { composePage } from "@store/helpers/compose";
import {
  FIXED_HEAD_POSITION_MP,
  setAnimation,
} from "@modules/menu/scrollAnimation/index";
import { menuLoadFailPoint, setFmpMenuListSuccess } from "./utils/point";
import {
  addDataUpdateListener,
  sendElseMenuListRenderDuration,
  sendInitPageToRequestStart,
  sendLVC,
  sendMenuListRenderDuration,
  sendRequestEndToDataStart,
  sendRequestStartToRequestEnd,
  sendSplashMenuFmpDuration,
  updateIsFirstEnter,
} from "./utils/perfLog";
import { PANEL_TYPE, PanelTransaction } from "@modules/panel/panelTransaction";
import { URLParams } from "@type/common/URLParameters";
import { getTabbarPageOptions } from "@main/lib/wx/util";
import { isNativeTabbarPage, setTabbarPageParams } from "@main/lib/wx/app-info";
import { setPointPurchase } from "@store/actions/point-purchase";
import { WEI_QIAN_TANENT_ID } from "@lib/wx/weiqian-special";
import { ICON_TYPE } from "@constants/decorConstants";

import MustDishSdk from "@modules/MustDishSdk";
import { TriggeredEvents as AdvertisingModalTriggeredEvents } from "../../../../components/limo-menu/advertising-modal/protocol";
const isAliPayNative = process.env.PLATFORM === "alipay";

const SEARCH_BANNER_SHOW = 1;
const SEARCH_BANNER_HIDE = 0;

const pageConfig = {
  pageViewModel: MpPageViewModel,
  // TrianglePage会调用, 一般页面只有一个首屏模块的不需要做额外定义
  defineFmpCondition: () => ({
    conditions: ["shopInfo", "recycleList"],
  }),
  // Limo,
  data: {
    showFullLoading: true,
    dishShowType: "",
    allDishList: [], // 切片后的菜���列表数据
    finishSetDishList: false,
    firstScreenCollectEnd: false,
    reachBoundary: "",
    menuScrollY: true,
    categoriesList: [], // 左侧菜品分类列表
    isMember: false,
    options: {},
    showOrderedDetailPanel: false, // 点过浮层状态
    expandedShopInfo: false, // 商家信息是否展开，展开是不展示购物车
    resultCode: 100,
    recycleWinWidth: 0,
    isShowBigBtn: true, // 菜单页展示添加菜品大按钮
    hasTabBar: false,
    initErrData: {},
    errorText: "",
    menuUpdateTime: 0,
    categoryStickyTop: FIXED_HEAD_POSITION_MP,
    hasEnoughOptions: false,
    location: {},
    isNativeTabbarPage: false,
    errorInfo: null,
    hideCartBarWhenEmpty: HIDE_CART_BAR_WHEN_EMPTY.menu,
  },
  isFirstShowToast: true, // 团购券使用张数限制，提示次数是否为第一次
  realCategoryCount: 0, //全量菜品的分类数统计，用来和首屏作对比
  isFirstSet: true, //是不是第一次触发列表渲染时间上报
  renderStartTime: 0, //列表开始渲染时间
  takeCouponData: {}, // 不参与渲染，避免浪费性能
  isDestroy: false,
  shareMsg: {},
  urlParams: {},
  isFirstEnterPage: true, // 是否第一次进入页面，进入其他页面之后再返回为false,不需要上报性能以及秒开率
  //时段菜信息弹窗组件参数
  dishTimeInfoModalRequestInfo: {
    requestHost: "",
    requestParams: {},
  },
  menuDetailScrollHeight: 0,
  async handleLoad(options: URLParams.MenuPage) {
    if (options && Object.keys(options).length) {
      await this._initPageShow(options);
    }
  },
  onPageScroll(info) {
    const scrollTop = info?.scrollTop;

    // 设置菜单列表是否到达边界
    if (
      scrollTop < this.menuDetailScrollHeight &&
      this.data.reachBoundary !== "top"
    ) {
      this.setData({
        reachBoundary: "top",
      });
    } else if (
      scrollTop >= this.menuDetailScrollHeight &&
      this.data.reachBoundary
    ) {
      this.setData({
        reachBoundary: "",
      });
    }
    //页面触顶校准
    if (scrollTop < 5 && this.data.reachBoundary !== "top") {
      this.setData({ reachBoundary: "top" });
      Limo &&
        Limo.setModuleData("menu-shop-info", {
          searchBannerOpacity: SEARCH_BANNER_HIDE,
        });
    }

    // 滚动过程中设置吸顶容器的sticky位置
    if (
      scrollTop >= this.stickyContainerScrollHeight &&
      !this.stickyContainerFixed
    ) {
      this.stickyContainerFixed = true;
      Limo && Limo.setModuleData("top-sticky-container", { fixed: true });
    } else if (
      scrollTop < this.stickyContainerScrollHeight &&
      this.stickyContainerFixed
    ) {
      this.stickyContainerFixed = false;
      Limo && Limo.setModuleData("top-sticky-container", { fixed: false });
    }
    if (!this.data.firstScreenCollectEnd) {
      this.setData({ firstScreenCollectEnd: true });
    }
    // 页面菜品滚动
    // 设置吸顶搜索 / 分类
    setAnimation(scrollTop, this, Limo);
  },
  onReachBottom() {
    this.setData({
      reachBoundary: "bottom",
    });
    Limo &&
      Limo.setModuleData("menu-shop-info", {
        searchBannerOpacity: SEARCH_BANNER_SHOW,
      });
  },
  switchCategory(event) {
    //锚定到正确分类
    const windowWidth = Triangle.MPInfo.getSystemInfo().windowWidth || 375;
    const categoryId = event.detail;
    const query = this.createSelectorQuery();

    query.selectViewport().scrollOffset();
    query.select(`.container >>> #item-${categoryId}`).boundingClientRect();
    query.exec((res) => {
      const nowScrollTop: number = (res[0] && res[0].scrollTop) || 0;
      const nowTabTop: number = (res[1] && res[1].top) || 0; // 菜品列表中当前item距离视口顶部的高度
      // 菜单分类在左侧
      const isDefaultCategory =
        this.data.dishShowType === DISH_ITEM_TYPE_VAL.DEFAULT ||
        this.data.dishShowType === DISH_ITEM_TYPE_VAL.RIGHT_LARGE;
      const headerHeight: number =
        // eslint-disable-next-line @typescript-eslint/restrict-plus-operands
        this.data.categoryStickyTop +
        ((isDefaultCategory ? 0 : 36) / 375) * windowWidth;
      wx.pageScrollTo({
        scrollTop: nowScrollTop + nowTabTop - headerHeight,
        duration: 0,
      });
    });
    //头部透明度
    Limo &&
      Limo.setModuleData("menu-shop-info", {
        searchBannerOpacity: SEARCH_BANNER_SHOW,
      });
  },
  //获取tabBar选中的type
  getSelectedPageType() {
    const options = this.urlParams;
    let selectedPageType;

    if (isTakeAwayOrSelfPick(options.bizType)) {
      selectedPageType =
        Number(options.bizType) === BIZ_TYPE_MAP.TAKEAWAY
          ? PORTAL_TAB_TYPE.TAKEAWAY
          : PORTAL_TAB_TYPE.PICKUP;
    } else {
      selectedPageType =
        Number(options.reserveMode) === DISH_SOURCE.RESERVE
          ? PORTAL_TAB_TYPE.PRE_ORDER
          : PORTAL_TAB_TYPE.ORDER_MENU;
    }
    return selectedPageType;
  },
  loadTabBar(options: URLParams.MenuPage) {
    let catchTabBarList = rmsStorage.getTabBarData(
      options.restaurantViewId || options.shopId,
    );
    const previewFlag = getNewApp().diancanGlobalData.getPreview(
      CONTAINER_NAME.TAB_BAR,
    );
    const requestParams = {
      params: {
        restaurantViewId: options.restaurantViewId || "",
        mtShopId: options.shopId || "",
        entrance: options.entrance || "",
        tenantId: options.tenantId || "",
        previewFlag,
        minaId: options.minaId || "",
        portalUrl: options.portalUrl || "",
        wmChannel: getParamsFromCookie("orderChannel") || -1,
      },
      url: getTabBarUrl(),
    };
    const selectedPageType = this.getSelectedPageType();

    if (previewFlag) {
      catchTabBarList = [];
      rmsStorage.setTabBarData(options.restaurantViewId || options.shopId, []);
    }
    this.setData({
      requestParams: catchTabBarList?.length ? {} : requestParams,
      catchTabBarList,
      selectedPageType,
    });
    return {
      isVirtualTabbar: !!catchTabBarList?.length,
    };
  },
  async onLoad(options: URLParams.MenuPage | { qrCode: string }) {
    initPageOnLoad();
    Limo.proxy({
      limoUseCustomHandler: {
        sendMC,
        sendPV,
        sendMV,
        Toast,
        Log,
        Triangle,
        ThemeUtil: {
          ICON_TYPE,
        },
        rmsStorage,
        appendExposeDishItem,
        exposeDishItemNew,
        dealOperationData,
        Cart,
        MustDishSdk,
        dealOperationCountStyle,
        advertiseReporter,
      },
    });
    options = getTabbarPageOptions(options, this.route);

    if (!this.optionsGuard(options)) {
      return;
    }

    const newOptions = {
      ...options,
      multiShop: options.multiShop || rmsStorage.getMultiShop(),
    };
    this.setData({
      options: newOptions,
      // Checking whether url parameter complete.
      hasEnoughOptions: hasEnoughOptions(newOptions),
      extraGetDecorationRequestParam: { shopCache: rmsStorage.getShopCache() },
      isNativeTabbarPage: !!isNativeTabbarPage(this.route),
    });
    updateIsFirstEnter(this.isFirstEnterPage);
    addDataUpdateListener(this);
    // 记录页面加载时间点，用于计算首屏时间
    this.pageLoadTime = Date.now();

    this.options = options;
    showMustDishToast(options);
    //从门店列表页长链接进入外卖菜单页场景。本地缓存下cacheKey
    this._updateCacheKey(options.cacheKey);

    Limo.registerEvent(
      "openAdSpuDetailEvent",
      (e: AdvertisingModalTriggeredEvents["openAdSpuDetailEvent"]) => {
        this.openAdvertiseSpuDetail(e);
      },
    );
    // 会员小黑条登录成功回调
    Limo.registerEvent("universalLogin-uniLoginSuccess", async () => {
      await this.handleLoad(this.urlParams);
    });

    await this.handleLoad(options);
  },

  checkWeiQian(options) {
    const { tenantId } = options;

    if (Number(tenantId) === WEI_QIAN_TANENT_ID) {
      const query = Triangle.stringify(options);
      rmsStorage.setMenuOps(query);
    }
  },
  /**
   * @description Update complete url parameter.
   * @param options complete url parameter object.
   */
  saveUrlParameters(options: URLParams.MenuPage) {
    // Checking whether url parameter complete.
    this.setData({ options, hasEnoughOptions: hasEnoughOptions(options) });
    // 味千门店需要缓存菜单页入参
    this.checkWeiQian(options);
    this.urlParams = options;
    this.setUrlParams(options);
    // Update options in case some components using getMixUrlParam() to get options information.
    this.options = options;
  },

  /**
   @description Check whether url parameter is correct.
   * @returns true - options is eligible to continue the following process.
   */
  optionsGuard(options) {
    if (this.checkOldPerception(options)) {
      // 虽然内部有跳转，还是阻止掉往下走的异常流程
      return false;
    }

    // If `options` only container `qrCode` property.
    if (
      isAliPayNative &&
      Object.keys(options).length === 1 &&
      "qrCode" in options
    ) {
      const app = getApp();
      app.qrCode = options.qrCode;

      this.loadFail(FAIL_REASON.MALFORMED_OPTIONS);

      wx.reLaunch({ url: "/pages/splash/index" });

      return false;
    }

    return true;
  },

  checkOldPerception(options) {
    // 路由逻辑已经统一到splash页面，兜底处理下可能的残留流量，参数中有 p 或者 q的跳走
    const { p, q } = options;
    if (p || q) {
      const query = Triangle.stringify(options, false);
      this.loadFail(FAIL_REASON.OLD_PERCEPTION);
      Triangle.redirectTo({ url: `/pages/splash/index?${query}` });
      return true;
    }
    return false;
  },

  async onShow() {
    sendPV("c_saaspay_gzi8cqe9");
    this.showTakeCouponTips = false;
    this.saveWMCloudData = false;

    if (!this.isFirstEnterPage) {
      this.setData({
        errorInfo: null,
      });
      let options = Object.assign(this.options, {});
      options = getTabbarPageOptions({}, this.route, options);
      this.saveUrlParameters(options);
      setTabbarPageParams(this.route, options);
      if (this.checkOldPerception(options)) return;
      this._updateCacheKey(this.options.cacheKey);
      await this.handleLoad(this.urlParams);
    }

    // 页面onShow时重新设置菜单固定头部透明度，menu-shop-info property中默认为0，频繁在菜单页和会员页间切换可能导致该值被重置，导致固定头部消失
    if (this.searchBannerOpacity) {
      Limo &&
        Limo.setModuleData("menu-shop-info", {
          searchBannerOpacity: this.searchBannerOpacity,
        });
    }
  },

  onReady() {
    const app = getNewApp();
    try {
      if (!app.isHide) {
        const coldStartDuration = Date.now() - app.report.launchTimeFromScan;
        Log.addPerformance(RAPTOR_PERFORMANCE.MP.APP_READY, coldStartDuration); // 小程序launch到页面首次渲染耗时，冷启动耗时减去这个耗时可以计算出包加载耗时
      }
    } catch (e) {}
    this.updateMpUserInfo();
  },

  updateComponentsScrollTop() {
    const query = this.createSelectorQuery();
    let categoryStickyTop = this.data.categoryStickyTop;
    let fixedHeadHeight = FIXED_HEAD_POSITION_MP;
    // 菜单固定头部
    query.select(`.container >>> .common-head-fixed`).boundingClientRect();
    // 时间条
    query.select(`.container >>> .top-sticky-container`).boundingClientRect();
    // 菜单列表
    query.select(`.container >>> .menu-detail`).boundingClientRect();
    // 页面滚动高度
    query.selectViewport().scrollOffset();
    query.exec((res) => {
      if (!Array.isArray(res)) return;
      const [
        commonHeadFixed = {},
        topStickyContainer,
        menuDetail = {},
        viewportScroll,
      ] = res;
      const viewportScrollTop = viewportScroll.scrollTop || 0; // 页面已滚高度
      // res[0]菜单固定头部
      if (commonHeadFixed && commonHeadFixed.height) {
        fixedHeadHeight = commonHeadFixed.height;
        this.setData({ categoryStickyTop: fixedHeadHeight });
      }
      // res[1]是时间条
      const { top, height } = topStickyContainer || {};
      if (top && height) {
        // 已经吸顶则不更新滚动高度
        if (top !== fixedHeadHeight) {
          this.stickyContainerScrollHeight =
            viewportScrollTop + top - fixedHeadHeight;
        }
        // 设置菜单分类的sticky位置
        categoryStickyTop = fixedHeadHeight + topStickyContainer.height;
        this.setData({ categoryStickyTop });
        // 设置吸顶容器的 sticky 位置
        Limo.setModuleData("top-sticky-container", {
          offsetTop: fixedHeadHeight,
        });
      }
      // res[2]是菜单列表的位置
      if (menuDetail && menuDetail.top) {
        this.menuDetailScrollHeight =
          viewportScrollTop + menuDetail.top - categoryStickyTop;
      }
    });
  },

  // 上报首屏时间、扫码耗时
  sendFstScanDuration() {
    const app = getNewApp();
    if (this.isFirstEnterPage) {
      // 菜单页onLoad-FMP
      this.loadSuccess("recycleList");
    }
    const { perceptionPage } = this.urlParams;
    if (perceptionPage) {
      // 新的FST
      sendSplashMenuFmpDuration(app.report?.splashLoadTime); // 新的fmp定义=splash的onLoad到menu的fmp
      app.report.splashLoadTime = -1;
    }
  },

  // 初始化 Limo 数据
  async initLimoData(limoCompsData, dealCoupons) {
    this.pageViewModel = new MpPageViewModel({
      pageInstance: this,
      Limo: Limo.getLimoRuntime(),
      options: this.urlParams,
    });
    const { shopId } = this.urlParams;
    // 初始化团购券数据，需要在第一次算价前完成
    await GrouponCouponSdk.initDealCoupons(
      shopId,
      this.pageViewModel,
      dealCoupons,
    );
    return this.pageViewModel.getMenuViewModel(limoCompsData);
  },

  _updateCacheKey(cacheKey: string) {
    cacheKey && setCloudDataCacheKey(cacheKey);
  },

  checkPointPurchase(fmpBizData) {
    if (fmpBizData?.pointBuyCampaign?.campaignAndSkuRelations) {
      this.setPointPurchase(
        fmpBizData.pointBuyCampaign.campaignAndSkuRelations,
      );
    }
  },

  async _initPageShow(options: URLParams.MenuPage) {
    const initPageShowStart = Date.now();
    const _requestMenuDataTime = sendInitPageToRequestStart(initPageShowStart);

    const { bizType, authGeo, reserveMode } = options;

    let location;
    // 自提，外卖和点餐叫号取餐开了距离判断的情况下需要获取用户的本地地址，或者 authGeo === '1'.
    if (
      isTakeAwayOrSelfPick(bizType) ||
      authGeo === URLParams.AuthGeo.REQUIRED
    ) {
      await transaction("MENU.getLocation", async () => {
        location = await getLocationInfo({
          canUseSessionCache: true,
          defaultTimeout: 500000,
          showAuthSettingDialog: false,
        });

        if (location) {
          this.setData({
            location,
          });
        }
      });
    }

    // 获取menulist数据
    const res: MenuMainData | ResponseState.REDIRECT = await getMainData({
      ...options,
      ...(location ? { userGeoPoint: location } : {}),
    });

    if (res === ResponseState.REDIRECT) {
      // Triangle.hideLoading();
      // Report redirect page status to avoid unexpected
      // timeout page status (sended when onUnload defined in triangle page.).
      this.loadFail(FAIL_REASON.REDIRECT);
      return;
    }

    const {
      pageTitle, // 标题
      memberInfo,
      localHeadInfo,
      spuDetail, // 非菜品数据,为了方便limo敏捷，这里的部分模型需要转换成旧的模型
      errorTips,
      spuPromise,
      moduleData,
      moduleSortList,
      userInfo,
      orderProductionProcessVO,
    } = res;
    const requestEnd = sendRequestStartToRequestEnd(_requestMenuDataTime);

    const { shopConfig, shopInfo, fmpBizData } = localHeadInfo ?? {};
    const {
      recSceneFlag,
      shopCache,
      showCouponPackage = false,
    } = fmpBizData ?? {};
    // Complete/update url parameters after got response data.
    const newOptions = getCompleteParameters({
      shopConfig,
      fmpBizData,
      oldParameter: options,
    });

    this.checkPointPurchase(fmpBizData);

    // Will assign newOption to this.urlParams.
    this.saveUrlParameters(newOptions);
    setTabbarPageParams(this.route, newOptions);

    if (this.isDestroy) {
      // 页面redirectTo到其他页面，已经销毁了，即调用了onUnLoad,防内存泄露
      return;
    }

    if (errorTips) {
      this.dealErrorInfo(errorTips);
      return;
    }

    if (!localHeadInfo) {
      this.dealErrorInfo({
        errorType: FAIL_REASON.EMPTY_HEAD,
        errorMsg: "localHeadInfo为空",
      });
      return;
    }

    // Query tab data after getting fmp data.
    const { isVirtualTabbar = false } = this.loadTabBar(newOptions);
    const { shopId, tableNum, spuId, cacheKey } = this.urlParams;
    this._updateCacheKey(cacheKey);

    this.localHeadInfo = localHeadInfo;
    // 设置页面标题栏商户名称
    // 返回场景下没有loading态阻止交互，该代码执行时可能用户已经去了其他页面
    if (isAliPayNative && my.canIUse("hideBackHome")) {
      // 支付宝小程序隐藏home
      my.hideBackHome();
    } else {
      setNavigationBarTitle({ title: pageTitle }, "/menu/");
    }
    // Sava location info to cloud store for order confirm web page to use in pick bizMode.
    saveLocationInfo({ bizType, cacheKey, location: this.data.location });

    Log.setAsyncOptions("shopId", shopId);
    clearLastShopInfo(newOptions);
    this.setExtraInfo(shopId, localHeadInfo);
    this.updateBaseInfo(shopId, tableNum);
    showMemberLoginToast(memberInfo);
    saveCardInfo(shopId, memberInfo);
    // Update `recSceneFlag` when it isn't `null`.
    if (recSceneFlag !== null) {
      rmsStorage.setRecSceneFlag(Boolean(recSceneFlag));
    }

    if (shopCache) {
      rmsStorage.setShopCache(shopCache);
    }

    const { menuStyle, spuMatchRecommendPages, mtShopId } = shopConfig || {};
    const { dishShowType = 0, spuComboShowType = 0 } = menuStyle || {};
    const horizontal =
      dishShowType > 0 && dishShowType !== DISH_SHOW_TYPE.RIGHT_LARGE;
    let column = 1;

    if (dishShowType === DISH_SHOW_TYPE.TWO_IN_ROW) {
      column = 2;
    } else if (dishShowType === DISH_SHOW_TYPE.THREEE_IN_ROW) {
      column = 3;
    }

    const { groupCouponInfo, shopMarketings } = shopInfo || {};
    const { showMTBind, dealCoupons, mtUserName } = groupCouponInfo || {};
    const setDataStart = sendRequestEndToDataStart(requestEnd);
    const limoMenuData = await this.initLimoData(
      { moduleData, moduleSortList, progressInfo: orderProductionProcessVO },
      dealCoupons,
    );

    //登录后提示是否领取成功
    if (this.showTakeCouponTips && moduleData) {
      showTakeCouponTipsHandle(moduleData, memberInfo, () => {
        this.showTakeCouponTips = false;
      });
    }

    const waiMaiConfig = moduleData?.["menu-shop-info"]?.data?.waiMaiConfig;
    if (waiMaiConfig) {
      this.updateTakeawayConfigInfo(waiMaiConfig.takeAwayDeliveryFeeRule);
    }

    const renderData = {
      limoMenuData,
      userInfo,
      memberInfo, // 门店标签、公告、logo、名称、展开后、会员 limo
      actionUrl: memberInfo?.actionUrl,
      pageTitle,
      horizontal,
      showMTBind,
      mtUserName,
      spuMatchRecommendPages,
      column,
      recycleWinWidth: transformRpxToNumber(
        MENU_PAGE_INIT_CONFIG[
          `INIT_${DISH_ITEM_TYPE_MAP[dishShowType]}_RECYCLE_VIEW_WIDTH`
        ],
      ),
      spuComboShowType,
      menuUpdateTime: Date.now(),
      currentShopId: mtShopId, // 通过缓存拿到的门店数据，链接上无shopId
      // 商家小程序  && 扫码点餐/自提 才支持装修展示开关
      showCouponPackage:
        menuStyle?.feConfig?.showPayCouponPackage === "false"
          ? false
          : showCouponPackage,
      payCouponDecoInfo: {
        // 付费券包装修数据
        couponPageSize:
          menuStyle?.feConfig?.couponPageSize &&
          Number(menuStyle?.feConfig?.couponPageSize),
        payCouponPackageShowImgUrl:
          menuStyle?.feConfig?.payCouponPackageShowImgUrl,
        sizeScale: menuStyle?.feConfig?.couponPageSizeScale,
      },
      shopMarketings,
    };
    transaction(
      "MENU.SET_DATA.SHOP_INFO",
      async (t) => {
        this.setData(renderData, () => {
          t.success();
          // 除了菜品长列表的菜单页信息
          if (this.isFirstEnterPage) {
            this.loadSuccess("shopInfo");
          }
          sendElseMenuListRenderDuration(setDataStart); // 非长列表渲染耗时，即setData耗时
          if (Triangle.isByteDanceMicroApp) {
            /**
             * TODO: by qiankeyu
             * 抖音中在setData回调中，无法通过createSelectorQuery直接取到limo-container中的元素，setTimeout一段时间后可以取到。
             * 先单独适配抖音小程序，后续再看是否能与微信保持一致。
             */
            setTimeout(() => {
              this.updateComponentsScrollTop();
            }, 200);
          } else {
            // 微信、支付宝走原逻辑
            this.updateComponentsScrollTop(); // 设置sticky组件的滚动高度
          }
          Limo && Limo.setModuleData("menu-shop-info", { isVirtualTabbar });
        });
      },
      true,
    );
    this.checkNewAdvanceToast();
    // 页面参数中包含spuId, 代表需要popup详情弹窗
    this.popUpDetailPanel(spuId);
    if (this.isFirstEnterPage) {
      this.dealRenderList(spuDetail, spuPromise);
    } else {
      this.updateMenuList(spuPromise);
    }
  },

  dealRenderList(spuMap, spuPromise) {
    const isFMP = !!spuPromise;
    const { categories, shopConfig, memberInfo, shopInfo, fmpBizData } =
      this.localHeadInfo;
    const menuStyle = shopConfig?.menuStyle;
    const isLogin = !!memberInfo?.cardId;
    const {
      recommendShowType = BOSS_DISH_SHOW_TYPE.DEFAULT,
      dishShowType = 0,
      menuType = 0,
    } = menuStyle || {};
    const shopMarketings = shopInfo?.shopMarketings || [];
    const { pointBuyCampaign } = fmpBizData;

    //”插件“数据整合到分类里
    const newCategories = getPluginActivityToCategory(
      categories,
      shopMarketings,
      spuMap,
    );

    const localCategories = generateCategoryList(isFMP, {
      categories: newCategories,
      dishShowType,
      list: spuMap,
      cartList: this.data.cartDishList,
      isLogin,
    });
    const categoryHasIcon = categoryListHasIcon(localCategories);
    // 菜品数据填充
    const dishShowTypeVal =
      DISH_ITEM_TYPE_MAP[dishShowType] || DISH_ITEM_TYPE_VAL.DEFAULT;

    const dishList = generateDishList(isFMP, {
      categories: localCategories,
      list: spuMap,
      dishShowType: dishShowTypeVal,
      cartList: this.data.cartDishList,
      menuStyle,
      pointBuyCampaign,
    });
    // 刷新分类列表
    this.setData({
      menuType,
      categoriesList: localCategories,
      categoryHasIcon: isFMP
        ? categories.some((category) => !!category.categoryMultimediaList)
        : categoryHasIcon,
      dishShowType: dishShowTypeVal,
      recommendShowType,
    });
    //记录下全量菜品的分类数，用来和首屏作比较
    if (!spuPromise) {
      this.realCategoryCount = dishList?.length || 0;
    }
    this.renderDishList(dishList, spuPromise);
  },

  // 渲染菜单长列表
  renderDishList(dishList, spuPromise) {
    this.renderStartTime = this.isFirstEnterPage ? Date.now() : 0; // 参考sendPerfLog
    // setContent 临时数据 放了循环 setData
    const setContent = sliceDataTransfer(dishList);
    // console.log('setContent====', setContent);
    // const start_time = Date.now();
    let count = 0;
    setContent.forEach((item) => {
      this.setData(item, () => {
        count++;
        if (this.isFirstSet) {
          // 上报首屏时间、扫码耗时
          this.sendFstScanDuration();
          // 首屏列表渲染耗时
          sendMenuListRenderDuration(this.renderStartTime);
          // 首屏菜品渲染成功
          setFmpMenuListSuccess();
          this.isFirstSet = false;
        }
        if (count === setContent.length) {
          // [setData打点]初始化 dishList 的 setData 完成
          // console.log('allDishList====', this.properties.allDishList, Date.now() - start_time);
          this.updateMenuList(spuPromise);
          this.removeExtraCategories();
        }
      });
    });
  },
  //删除首屏可能多余的分类，融合项目后可删除该段逻辑
  removeExtraCategories() {
    if (
      this.realCategoryCount > 0 &&
      this.data.allDishList.length > this.realCategoryCount
    ) {
      for (
        let i = this.realCategoryCount;
        i < this.data.allDishList?.length;
        i++
      ) {
        this.setData({
          [`allDishList[${i}]`]: null,
        });
      }
    }
  },

  async updateMenuList(spuPromise) {
    if (!spuPromise) {
      // All pageSpuInfo data is loaded.
      // Update the `menuUpdateTime` trigger the `recommendation-module` uses newest dish data.
      Limo &&
        Limo.setModuleData("recommendation-module", {
          menuUpdateTime: Date.now(),
        });
      // 关闭页面loading
      // Triangle.hideLoading();
      if (this.isFirstEnterPage) {
        sendLVC(this.pageLoadTime);
      }
      this.setData({
        showFullLoading: false,
        finishSetDishList: true,
      });

      return;
    }
    const { shopId, tenantId, mandatoryPrompt } = this.urlParams;
    const { shopConfig, shopInfo } = this.localHeadInfo;

    let allSpu;
    try {
      allSpu = await getAllSpuWithCache(this.urlParams, spuPromise);
    } catch (e) {
      Log.addError("全量菜品网络异常", e);
    }
    if (!allSpu) {
      this.dealErrorInfo({
        errorType: FAIL_REASON.ALL_DISH_FAIL,
        errorMsg: DEFAULT_ERROR_TITLE,
      });

      return;
    }

    const { mandatoryInfos, cartConfig } = shopConfig || {};
    this.setDishList(shopId, allSpu);
    rmsStorage.setUseMenuCacheForNonMenuPage(shopId, true); // 搜索页可以使用全量菜作为缓存
    this.initNewCart(
      mandatoryInfos,
      allSpu,
      cartConfig?.openTogether,
      decodeURIComponent(mandatoryPrompt ?? ""),
    );
    this.dealRenderList(allSpu, null); // 刷新完整菜单后，promise传null,不用再刷新了
    GrouponCouponSdk.showMTAccountToast(
      shopInfo,
      rmsStorage.getGrouponCouponList(shopId),
    );
    this.shopHomeShare(shopId, tenantId);
    initUser();
  },

  dealErrorInfo(e: ErrorTips) {
    // Triangle.hideLoading();
    const { errorType, errorMsg } = e || {};
    const pageType = this.getSelectedPageType();
    menuLoadFailPoint({ errorType, errorMsg });
    this.loadFail(errorType);
    //https://p0.meituan.net/travelcube/10ad2114d0910ffe72dc8c2afd7574b037900.png
    const errPageInfo = getErrPageInfo(errorType);
    let type =
      errorType === ERROR_TYPE.NEED_DOWNGRADE_CODE
        ? errorType
        : ERROR_TYPE.NEED_RELOAD;
    let errUrl = "";
    let btnText =
      errorType === ERROR_TYPE.NEED_DOWNGRADE_CODE ? "去点餐" : "重新加载";
    if (errPageInfo.type) {
      type = errPageInfo.type;
      errUrl = errPageInfo.url;
      btnText = errPageInfo.btnText;
    }
    this.setData({
      errorInfo: {
        errorTitle: errorMsg,
        btnConfig: {
          buttonText: btnText,
        },
        redirectUrl: errUrl || "",
        type,
      },
      // btnText,
      // url: errUrl,
    });
    // toErrorTip({
    //   type,
    //   errorTip: errorMsg,
    //   pageType,
    //   url: errUrl,
    //   btnText,
    // });
  },

  handleErrorClick() {
    const { errorInfo } = this.data;
    const { type, redirectUrl } = errorInfo || {};
    switch (+type) {
      case ERROR_TYPE.NEED_DOWNGRADE_CODE: {
        const pagePath = getCurrentPagePath();
        this.openIndependentApplets(pagePath);
        return;
      }
      case ERROR_TYPE.CUSTOM_JUMP: {
        Triangle.redirectTo({
          url: redirectUrl,
        });
        return;
      }
      case ERROR_TYPE.NEED_RELOAD:
      default: {
        const params = getTakeawayUrlParams();
        const { multiShop } = params || {};
        customReLaunch("menu", {
          multiShop,
        });
        return;
      }
    }
  },

  openIndependentApplets(targetPage) {
    // wx1fde2c33280d64b6 线上
    // wx06a469c9bcc9f1ea 线下
    const appId =
      checkEnv() === "develop" ? "wx06a469c9bcc9f1ea" : "wx1fde2c33280d64b6";
    wx.navigateToMiniProgram({
      appId,
      path: targetPage,
      extraData: {},
      envVersion: checkEnv(), // 环境设置，联调时跳开发版
      success: (res) => {
        console.log("小程序打开成功", res);
      },
      fail: (error) => {
        console.log("小程序打开失败", error);
        Log.addError(`小程序打开失败`, error);
      },
    });
  },

  checkNewAdvanceToast() {
    const { orderBizTag, hasOrder } = this.urlParams;
    if (+orderBizTag === ORDER_BIZ_TAG.AHEAD && hasOrder === "true") {
      Triangle.showToast({
        icon: "none",
        title: "该桌台有订单，请您联系服务员或更换桌台用餐",
        duration: 3000,
      });
    }
  },

  async shopHomeShare(shopId, tenantId) {
    const options = this.urlParams;
    const bizType = Number(options.bizType);
    let sharePageSource;
    if (bizType === BIZ_TYPE_MAP.PICKUP || bizType === BIZ_TYPE_MAP.TAKEAWAY) {
      sharePageSource = bizType;
    } else {
      sharePageSource = Number(options.reserveMode)
        ? SHARE_INFO_SOURCE.PRE_ORDER
        : SHARE_INFO_SOURCE.ORDER;
    }
    const params = {
      containerName: "shop-share-config",
      previewFlag: false,
      bizId: shopId,
      bizIdType: "10",
      tenantId,
      sharePageSource,
    };
    try {
      const shareConfig = await shopApi.getShopHomeShare(params);
      const { shareMsg, shareSwitch } = shareConfig;
      if (shareSwitch) {
        this.shareMsg = shareMsg;
      } else {
        wx.hideShareMenu({ menus: ["shareAppMessage"] });
      }
    } catch (error) {
      console.log(error);
    }
  },

  onShareAppMessage(e) {
    const { species } = e?.target?.dataset || {};
    if (e?.from === "button" && species === DISH_SHARE_TYPE)
      return this.dishShareInfo;
    return this.shareMsg;
  },

  getSpuById(spuId) {
    const mtShopId = this.urlParams.shopId;
    if (!spuId || !mtShopId) {
      return null;
    }
    const dishList = rmsStorage.getDishList(mtShopId);
    if (!dishList) {
      return null;
    }
    return dishList[spuId];
  },

  // 将 app 上的头像昵称信息更新到页面上
  updateMpUserInfo() {
    const { nickname, headimgurl } = getUserInfo();
    const currMpUserInfo = { nickname, headimgurl };
    this.setMpUserInfo(currMpUserInfo);
  },
  popUpDetailPanel(spuId) {
    const spu = this.getSpuById(spuId);
    // 菜品禁售
    // 或参数存在spuId, 但未找到菜品
    const itemSoldOut = dealPreventSale(spu) || (!spu && spuId);
    const { alreadyPopup } = this.data;
    if (!alreadyPopup && spuId) {
      if (itemSoldOut) {
        Triangle.showToast({
          icon: "none",
          title: "菜品已售完, 看看其他菜品吧~",
          duration: 3000,
        });
      } else {
        this.menuDetail({
          detail: spu,
        });
      }

      this.setData({
        alreadyPopup: true,
      });
    }
  },
  // 打开菜品详情 - 老板推荐打开详情
  menuDetail(event) {
    const dish = event.detail;
    if (!dish) return;
    const spuId = dish.spuId;
    const { shopId } = this.urlParams;
    const extraInfo = rmsStorage.getExtraInfo(shopId) || {};
    const bossRecommendText = extraInfo.shopInfo?.recommendInfos?.boss?.title;
    this.openDishDetail({
      ...dish,
      reportConfig: {
        ...(dish.reportConfig || {}),
        ...(dish.__reportConfig__ || {}),
        bossRecommendText,
        dishType: dish.dishType,
        spuName: dish.spuName,
        adFrom: EXPOSE_DISH_TYPE.BOSSRECOMMEND,
        spuId,
        skuId: dish.skuMenuItems ? dish.skuMenuItems[0]?.skuId : 0,
      },
    });
    const paramsCommon = {
      op_type: OP_TYPE.RECOMMEND_DISH, // 功能区名称
      op_name: dish.__reportConfig__?.fromNetRecommend
        ? OP_NAME.RECOMMEND_NET
        : OP_NAME.RECOMMEND_BOSS, // 功能名称
      sn: dish.__reportConfig__?.index, // 序号
      show_title: dish.__reportConfig__?.fromNetRecommend
        ? TITLE_CONTENT.NET
        : TITLE_CONTENT.BOSS, // 展示标题
      spu_id: spuId,
      skuId: dish?.skuMenuItems[0]?.skuId,
    };
    let paramsFinal: DishDetailPointParams = { ...paramsCommon };
    // 老板推荐时添加is_defined_title参数
    if (!dish.__reportConfig__?.fromNetRecommend) {
      const isDefinedTitle = bossRecommendText ? 1 : 0;
      paramsFinal = {
        is_defined_title: isDefinedTitle,
        ...paramsCommon,
        show_title: bossRecommendText,
      };
    }

    // 新增菜品详情埋点
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      clickData: {
        ...paramsFinal,
      },
    });
  },
  // 统一加菜
  addDishFuncMenu(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.addDish(skuDish);
  },
  onClickCountNum(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.showCalcPanel(skuDish);
  },
  // 统一减菜
  minusDishFuncMenu(event) {
    const spuDish = event.detail;
    const skuDish = transToSkuDish(spuDish, this.data.cartDishList);
    this.minusDish(skuDish);
  },
  // 监听头部商家详情的展开情况
  expandShopInfo(e) {
    const { showShopFlag } = e.detail;
    this.setData({
      expandedShopInfo: showShopFlag,
      isShowToastFromMTLogin: false,
    });
  },
  hidePanel() {
    this.toggleRegisterPanelAction(false);
    this.toggleContainerPanelAction(false);
    this.setAddOnShow(false);
    this.closeTakeCouponPanel();
  },
  onHide() {
    this.isFirstEnterPage = false;
    updateIsFirstEnter(this.isFirstEnterPage);
    this.hidePanel();
    this.updateCloudCacheCartData();
    this.saveWMCloudData = true;
  },
  onUnload() {
    this.isDestroy = true;
    this.hidePanel();
    //onHIde生命周期执行完之后。onUnload内不用再执行一次了
    if (!this.saveWMCloudData) {
      this.updateCloudCacheCartData();
    }
    // 跳走时可能有没关的
    Triangle.hideLoading();
  },
  showPanelType(e) {
    const {
      operationData = {},
      skuId = "",
      __newReportConfig__,
      exposeDishType = "",
      menuReportConfig,
    } = e.detail;
    // 处理关联菜品逻辑
    const { panelType, id, reportConfig, discount = "" } = operationData;
    console.log("operationData==", operationData);
    if (panelType && id) {
      const containerOpenExtraData = {
        reportConfig: {
          ...(menuReportConfig || {}),
          ...reportConfig,
        },
      };
      this.updateCurrentDishId({
        currentSpuId: id,
        currentSkuId: skuId,
        containerOpenExtraData,
        __newReportConfig__,
        currentDiscount: discount,
      });
      this.toggleContainerPanelAction(true);
      this.setDishAddFromAction(exposeDishType);
      // 【Panel触发】根据PanelType打开container-panel中不同的panel，包括多规格、套餐、计算器
      switch (panelType) {
        case PANEL_TYPE_VAL.SPEC:
          PanelTransaction.init(PANEL_TYPE.MULTI_PANEL);
          break;
        case PANEL_TYPE_VAL.PACKAGE:
          PanelTransaction.init(PANEL_TYPE.PACKAGE_PANEL);
          break;
        case PANEL_TYPE_VAL.WEIGHT:
          PanelTransaction.init(PANEL_TYPE.CALCULATOR_PANEL);
          break;
        default:
          break;
      }
    }
  },
  // 菜品详情 - 菜单打开
  showDishPanel(e) {
    const spuId = e.detail.dish.spuId;
    const isFirstScreen = e.detail.isFirstScreen;
    const { dish } = e.detail;
    const spu = this.getSpuById(spuId);
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      // 菜品图片点击埋点
      tab_id: dish.categoryId,
      dishes_id: dish.spuId,
      dishes_name: dish.name,
      dishes_price: Number(dish.currentPrice),
      dishes_status: spu.soldOut ? 1 : spu.canSaleNow ? 0 : undefined,
    });
    if (!spu || !dish) {
      return;
    }
    const { shopId } = this.urlParams;
    const extraInfo = rmsStorage.getExtraInfo(shopId);
    const bossRecommendText = extraInfo?.shopInfo?.recommendInfos?.boss?.title;
    this.openDishDetail({
      ...spu,
      reportConfig: {
        ...(dish.reportConfig || {}),
        ...(dish.__reportConfig__ || {}),
        bossRecommendText,
        dishType: dish.dishType || spu.dishType,
        spuName: dish.spuName || spu.spuName,
        adFrom: EXPOSE_DISH_TYPE.DISHLIST,
        spuId,
        skuId: dish.skuMenuItems ? dish.skuMenuItems[0]?.skuId : 0,
        categoryId: dish.categoryId,
        isFirstScreen,
      },
    }); // 补充菜品详情打点需要的参数
    // 【Panel触发】菜单列表上打开菜品详情面板
    PanelTransaction.init(PANEL_TYPE.DISH_DETAIL_PANEL);
    let opName = OP_NAME.OTHER;
    let showTitle = "";
    // categoryId大于0是分类推荐
    if (dish.categoryId > 0) {
      opName = OP_NAME.RECOMMEND_CLASS;
    } else {
      opName = OP_NAME.RECOMMEND_DISH;
    }
    showTitle = dish.__reportConfig__?.belongCategory?.parentName;

    // 新增菜品详情埋点
    sendMC("b_saaspay_hlznku1a_mc", null, null, {
      clickData: {
        op_type: OP_TYPE.RECOMMEND_CLASS, // 功能区名称
        op_name: opName, // 功能名称
        show_title: showTitle, // 展示标题
        spu_id: spuId,
        sn: dish.__reportConfig__?.index, // 序号
      },
    });
  },

  addToCartCoupon(e) {
    const { dish } = e.detail;
    const { spuId, skuId, count } = dish;
    GrouponCouponSdk.autoUseCouponAddToCart(
      spuId,
      skuId,
      count,
      () => {
        this.addToCart(dish);
      },
      () => {
        this.addToCart(dish);
      },
    );
  },
  operateCartDish(e) {
    const { type, id, showCal = false, skuId = "", discount = "" } = e.detail;
    const spu = this.getSpuById(id);
    if (!spu) {
      return;
    }
    // 积分购
    const isPointPurchase = !!(discount === POINT_DISH_TYPE);

    const dish = transToSkuDish(
      spu,
      this.data.cartDishList,
      skuId,
      isPointPurchase,
    );
    if (!dish) {
      return;
    }

    if (showCal) {
      this.updateCurrentDishId({
        currentSpuId: id,
        currentDiscount: discount,
      });
      this.toggleContainerPanelAction(true);
      this.updateCalculator({
        isShowCountCalc: true,
      });
      this.updateEditSkuDishAction({
        editDishSpuId: dish.spuId,
        editDishGoodsNo: dish.goodsNo,
      });
      return;
    }
    if (type === "plus") {
      if (isPointPurchase) {
        this.pointPurchaseAdd(dish);
        return;
      }

      GrouponCouponSdk.autoUseCouponAddToCart(
        dish.spuId,
        dish.skuId,
        1,
        () => {
          this.addDish(dish);
        },
        (singleSpu) => {
          if (singleSpu.dishType === DISH_TYPE.PACKAGE) {
            // 如果团购券对应的菜品为固定套餐，无需弹出套餐面板，直接加入购物车
            if (judgeFixedPackage(singleSpu)) {
              this.addFixedPackage(singleSpu, this.data.refactorLocalCart);
            } else {
              this.updateCurrentDishId({
                currentSpuId: singleSpu.spuId,
              });
              this.toggleContainerPanelAction(true);
            }
          } else {
            if (judgeComplexSpuDish(singleSpu)) {
              this.updateCurrentDishId({
                currentSpuId: singleSpu.spuId,
              });
              this.toggleContainerPanelAction(true);
            } else {
              const singleSkuDish = Immutable.getIn(singleSpu, [
                "skuMenuItems",
                0,
              ]);
              this.addDish(singleSkuDish);
            }
          }
        },
      );
    } else if (type === "minus") {
      // if (isPointPurchase) {
      //   this.pointPurchaseMinus(dish);
      //   return;
      // }
      this.minusDish(dish);
    }
  },
  tabBarRenderFn(e) {
    const { tabBarList = [] } = e.detail;
    if (!this.data.hasTabBar && tabBarList.length) {
      Limo && Limo.setModuleData("menu-shop-info", { isVirtualTabbar: true });
      this.setData({
        hasTabBar: true,
      });
    }
    const { requestParams } = this.data;
    const { restaurantViewId = "", mtShopId = "" } = requestParams.params || {};
    if (restaurantViewId || mtShopId) {
      rmsStorage.setTabBarData(restaurantViewId || mtShopId, tabBarList);
    }
  },
  openAdvertiseSpuDetail(
    event: AdvertisingModalTriggeredEvents["openAdSpuDetailEvent"],
  ) {
    const { exposeDishType, adType, spu, reportConfig } = event;
    this.setDishAddFromAction(exposeDishType);
    if (adType === AD_TYPE.SPU) {
      if (!spu) {
        return;
      }
      this.openDishDetail({
        ...spu,
        reportConfig,
      });
    }
  },
  showDishTimeInfoModalFn(e) {
    const { spuId } = e.detail;
    this.showDishTimeInfoModal(spuId);
  },
  dishTimeInfoModalEvent() {
    this.toggleDishInfoModal(false);
  },
  gotoThirdLogin() {
    // this.showLoadingOnShow = true;
    this.pageViewModel.gotoThirdLoginClick();
  },
  useGrouponCoupon(e) {
    const { discountTempId, spuId, skuId } = e.detail || {};
    if (!discountTempId || !spuId || !skuId) return;
    const { shopId } = this.urlParams;
    const dishList = rmsStorage.getDishList(shopId);
    if (!dishList) {
      return;
    }
    const spuDish = dishList[spuId];
    const skuDish = spuDish?.skuMenuItems?.find((item) => item.skuId === skuId);
    GrouponCouponSdk.addGrouponCouponDishToCart(
      [discountTempId],
      spuDish,
      skuDish,
      false, // 手动使用团购券，非自动
      (singleSpu) => {
        if (singleSpu.dishType === DISH_TYPE.PACKAGE) {
          if (judgeFixedPackage(singleSpu)) {
            this.addFixedPackage(singleSpu, this.data.refactorLocalCart);
          } else {
            this.updateCurrentDishId({ currentSpuId: singleSpu.spuId }); // 当前选中菜品的spuId
            this.toggleContainerPanelAction(true);
          }
        } else {
          if (judgeComplexSpuDish(singleSpu)) {
            this.updateCurrentDishId({ currentSpuId: singleSpu.spuId });
            this.toggleContainerPanelAction(true);
          } else {
            const dish = Immutable.getIn(singleSpu, ["skuMenuItems", 0]);
            this.addDish(dish); // 加菜的逻辑
          }
        }
      },
    );
  },
  cancelGrouponCoupon(e) {
    const { discountTempId, spuId } = e.detail || {};
    const minusGoodsNo = GrouponCouponSdk.cancelDealCoupon(discountTempId);
    minusGoodsNo && this.minusCartGrouponDish(spuId, minusGoodsNo);
  },
  //关闭领券弹窗
  closeTakeCouponPanel() {
    this.showTakeCouponPanelAction({
      takeCouponPanelTag: false,
    });
  },
  // 弹领券浮层
  openCouponPanel(e) {
    this.showTakeCouponPanelAction({
      takeCouponPanelTag: true,
      sourceType: e.detail.from,
    });
  },
  async handleTakeCoupon() {
    this.showTakeCouponTips = true;
    await this._initPageShow(this.urlParams);
  },
  // 根据分类id更新allDishList
  updateDishList(e) {
    const { categoryId } = e.detail;
    const setObj = getSetObj(
      this.data.allDishList,
      categoryId,
      this.data.dishShowType,
    );
    this.setData(setObj);
  },
  // 根据modifyObj的key和value更新allDishList
  modifyDishList(e) {
    const { modifyObj } = e.detail;
    if (!modifyObj) return;
    this.setData(modifyObj);
  },
  selectedAddressFn(e) {
    const { address } = e.detail;
    this.selectAddress(address);
  },
  handleAddNewAddressFn() {
    const { fmpBizData = {} } = this.localHeadInfo;
    const { addressPageUrl = "" } = fmpBizData;
    if (addressPageUrl) {
      Triangle.navigateTo({ url: addressPageUrl });
    }
  },
  closeAddressListModal() {
    this.setAddressListModalFlag(false);
  },
  getUserAddressList() {
    this.getUserAddressList();
  },
  updateAddressFn(e) {
    const { addressList = [] } = e.detail;
    this.setUserAddressList(addressList);
  },
  toggleShop(e) {
    const { bizType } = e.detail;
    viewOtherShop(bizType);
  },
  async switchBiz(e) {
    const { bizTypeStatus } = e.detail;
    // 切换自提和外卖时清空购物车
    this.clearCart();
    const { fmpBizData = {} } = this.localHeadInfo;
    const { addressPageUrl = "" } = fmpBizData;
    await switchBiz(bizTypeStatus, addressPageUrl);
  },
  updatePickUpDistanceFn() {
    this.updatePickUpDistance();
  },

  async onTapTipsBannerButton(event) {
    await handleTapTipsBannerButton({
      event,
      onGotLocation: (location) => {
        if (this.urlParams.authGeo !== URLParams.AuthGeo.REQUIRED) {
          // Reason to change authGeo on the query to REQUIRED:
          //  In situations like merchant opens the order distance restriction switch when the menu page already gets the page query.
          //  The authGeo parameter on the query doesn't equal '1',
          //  but the back end needs location info to verify the order distance.
          this.saveUrlParameters({
            ...this.urlParams,
            authGeo: URLParams.AuthGeo.REQUIRED,
          });
        }

        this.setData({ location });
        // Reload page.
        // Because the getLocationInfo() will store the location information into storage
        // and use stored location information after reloading.
        // Triangle.showLoading({ title: '加载中' });
        this._initPageShow(this.urlParams);
      },
      Limo,
    });
  },

  closeOrderProgress() {
    rmsStorage.setOrderProgress(this.urlParams.shopId);
  },
  updatePrePriceTipsFn() {
    if (this.data.showPrePriceTips) {
      this.updatePrePriceTips(false);
    }
  },
};

const mapStateToData = (state) => ({
  cartDishList:
    Immutable.getIn(state, ["cart", "cartDishList"]) || Immutable({}),
  cartDishSortMapList:
    Immutable.getIn(state, ["cart", "cartDishSortMapList"]) || Immutable([]),
  spuDish: Immutable.getIn(state, ["cart", "spuDish"]),
  mpUserInfo: Immutable.getIn(state, ["extraInfo", "mpUserInfo"]),
  showContainerPanelTag: Immutable.getIn(state, [
    "cart",
    "showContainerPanelTag",
  ]),
  showDishInfoModal: Immutable.getIn(state, ["panel", "showDishInfoModal"]),
  dishTimeInfoModalRequestInfo: Immutable.getIn(state, [
    "panel",
    "dishTimeInfoModalRequestInfo",
  ]),
  refactorLocalCart: Immutable.getIn(state, ["cart", "refactorLocalCart"]),
  takeCouponPanelInfo:
    Immutable.getIn(state, ["panel", "takeCouponPanelInfo"]) || {},
  takeCouponPanelData:
    Immutable.getIn(state, ["panel", "takeCouponPanelData"]) || {},
  showPrePriceTips: Immutable.getIn(state, ["panel", "showPrePriceTips"]),
});

const mapDispatchToData = (dispatch) => ({
  // 积分购加减菜
  pointPurchaseAdd: (skuDish) => dispatch(PointPurchase.addDish(skuDish)),
  // pointPurchaseMinus: skuDish => dispatch(PointPurchase.minusDish(skuDish)),
  setPointPurchase: (campaignAndSkuRelations) =>
    dispatch(setPointPurchase(campaignAndSkuRelations)),
  addDish: (skuDish) => dispatch(addDish(skuDish)),
  addToCart: (skuDish) => dispatch(addToCart(skuDish)),
  minusDish: (skuDish) => dispatch(minusDish(skuDish)),
  minusCartGrouponDish: (spuId, goodsNo) =>
    dispatch(minusCartGrouponDish(spuId, goodsNo)),
  openDishDetail: (spuDish) => dispatch(openDishDetail(spuDish)),
  updateBaseInfo: (mtShopId, mtTableNum) =>
    dispatch(updateBaseInfo(mtShopId, mtTableNum)),
  setDishList: (mtShopId, dishList) =>
    dispatch(setDishList(mtShopId, dishList)),
  setExtraInfo: (mtShopId, headInfo) =>
    dispatch(setExtraInfo(mtShopId, headInfo)),
  // @ts-ignore: TODO: 扩张参数必须具有元祖类型或传递给rest参数
  initNewCart: (...args) => dispatch(Cart.init(...args)),
  setUrlParams: (params) => dispatch(setUrlParams(params)),
  setMpUserInfo: (params) => dispatch(setMpUserInfo(params)),
  showCalcPanel: (skuDish) => dispatch(showCalcPanel(skuDish)),
  updateCalculator: (calcInfo) => dispatch(updateCalculator(calcInfo)),
  editWeightSkuDish: (skuDish) => dispatch(editWeightSkuDish(skuDish)),
  // 多规格操作时--
  toggleRegisterPanelAction: (showRegisterPanel) =>
    dispatch(toggleRegisterPanelAction(showRegisterPanel)),
  updateCurrentDishId: (dishId) => dispatch(updateCurrentDishId(dishId)),
  toggleContainerPanelAction: (show) =>
    dispatch(toggleContainerPanelAction(show)),
  setDishAddFromAction: (dishAddFrom) =>
    dispatch(setDishAddFromAction(dishAddFrom)),
  updateEditSkuDishAction: (editSkuDishInfo) =>
    dispatch(updateEditSkuDishAction(editSkuDishInfo)),
  toggleDishInfoModal: (showDishInfoModal) =>
    dispatch(toggleDishInfoModal(showDishInfoModal)),
  showDishTimeInfoModal: (spuId) => dispatch(showDishTimeInfoModal(spuId)),
  showTakeCouponPanelAction: (takeCouponInfo) =>
    dispatch(showTakeCouponPanelAction(takeCouponInfo)),
  addFixedPackage: (menuSpu, refactorLocalCart) =>
    dispatch(Cart.addFixedPackage(menuSpu, refactorLocalCart)),
  setAddOnShow: (isShow) => dispatch(setAddOnShowAction(isShow)),
  selectAddress: (addressId) => dispatch(selectAddress(addressId)),
  getUserAddressList: () => dispatch(getUserAddressList()),
  setUserAddressList: (addressList) =
    dispatch(setUserAddressList(addressList)),
  setAddressListModalFlag: (tag) => dispatch(setAddress alFlag(tag)),
  updateTakeawayConfigInfo: (takeAwayDeliveryFeeRule) =>
    dispatch(updateTakeawayConfigInfo(takeAwayDeliveryFeeRule)),
  clearCart: () => {
    dispatch(Cart.clearCart());
    sendMC("b_saaspay_lxmzp99s_mc");
  },
  updatePickUpDistance: () => dispatch(updatePickUpDistance()),
  updateCloudCacheCartData: () => dispatch(updateCloudCacheCartData()),
  updatePrePriceTips: (tag) => dispatch(updatePrePriceTips(tag)),
});

  function bar(){
  dsf
  }
const nextPageConfig = composePage(
  mapStateToData,
  mapDispatchToData,
  pageConfig,
);

ProxyPage(nextPageConfig, plugins);
