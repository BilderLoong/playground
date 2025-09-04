// ==UserScript==
// @name         YouTube Subtitle Masker
// @namespace    http://tampermonkey.net/
// @version      1.0
// @description  Adds movable and resizable rectangular masks to the YouTube player to cover subtitles or other elements.
// @author       Birudo
// @match        *://*.youtube.com/watch*
// @grant        none
// @run-at       document-idle
// ==/UserScript==

(function () {
  "use strict";

  // JSDoc type definitions for clarity and type safety.
  /**
   * @typedef {'move' | 'resize-br' | 'resize-bl' | 'resize-tr' | 'resize-tl'} DragType
   */

  /**
   * @typedef {object} DragState
   * @property {string} maskId - The ID of the mask being dragged/resized.
   * @property {DragType} type - The type of drag operation.
   * @property {number} startX - The initial X coordinate of the mouse event.
   * @property {number} startY - The initial Y coordinate of the mouse event.
   * @property {number} originalX - The original X position of the mask.
   * @property {number} originalY - The original Y position of the mask.
   * @property {number} originalWidth - The original width of the mask.
   * @property {number} originalHeight - The original height of the mask.
   */

  /**
   * @typedef {object} Mask
   * @property {string} id
   * @property {number} x
   * @property {number} y
   * @property {number} width
   * @property {number} height
   * @property {number} zIndex
   */

  /**
   * @typedef {object} AppState
   * @property {Mask[]} masks - An immutable list of all mask objects.
   * @property {DragState | null} activeDrag - Information about the current drag operation, or null if none.
   */

  // ========================================================================
  //  CORE LOGIC & STATE MANAGEMENT (PURE FUNCTIONS)
  // ========================================================================

  /**
   * Creates the initial state of the application.
   * @returns {AppState} A new, empty application state.
   */
  const createInitialState = () => ({
    masks: [],
    activeDrag: null,
  });

  /**
   * Adds a new mask to the state.
   * @param {AppState} state - The current application state.
   * @returns {AppState} The new application state with an added mask.
   */
  const addMask = (state) => {
    const newMask = {
      id: `mask-${Date.now()}`,
      x: 25,
      y: 650,
      width: 1115,
      height: 41,
      zIndex: 100 + state.masks.length,
    };
    return {
      ...state,
      masks: [...state.masks, newMask],
    };
  };

  /**
   * Updates a specific mask's properties.
   * @param {AppState} state - The current application state.
   * @param {string} maskId - The ID of the mask to update.
   * @param {Partial<Mask>} updates - An object with properties to update.
   * @returns {AppState} The new application state with the updated mask.
   */
  const updateMask = (state, maskId, updates) => ({
    ...state,
    masks: state.masks.map((mask) =>
      mask.id === maskId ? { ...mask, ...updates } : mask
    ),
  });

  /**
   * Starts a drag or resize operation.
   * @param {AppState} state - The current application state.
   * @param {DragState} dragInfo - The details of the drag operation to start.
   * @returns {AppState} The new state with an active drag.
   */
  const startDrag = (state, dragInfo) => ({
    ...state,
    activeDrag: dragInfo,
  });

  /**
   * Ends any active drag operation.
   * @param {AppState} state - The current application state.
   * @returns {AppState} The new state with no active drag.
   */
  const endDrag = (state) => ({
    ...state,
    activeDrag: null,
  });

  /**
   * Calculates the new position and size of a mask during a drag event.
   * This is the core transformation logic.
   * @param {AppState} state - The current application state.
   * @param {{clientX: number, clientY: number}} mousePos - The current mouse position.
   * @returns {AppState} The new state with the mask moved/resized.
   */
  const handleDrag = (state, mousePos) => {
    if (!state.activeDrag) return state;

    const {
      maskId,
      type,
      startX,
      startY,
      originalX,
      originalY,
      originalWidth,
      originalHeight,
    } = state.activeDrag;
    const dx = mousePos.clientX - startX;
    const dy = mousePos.clientY - startY;

    const newProps = (() => {
      switch (type) {
        case "move":
          return { x: originalX + dx, y: originalY + dy };
        case "resize-br":
          return {
            width: Math.max(20, originalWidth + dx),
            height: Math.max(20, originalHeight + dy),
          };
        case "resize-bl":
          return {
            x: originalX + dx,
            width: Math.max(20, originalWidth - dx),
            height: Math.max(20, originalHeight + dy),
          };
        case "resize-tr":
          return {
            y: originalY + dy,
            width: Math.max(20, originalWidth + dx),
            height: Math.max(20, originalHeight - dy),
          };
        case "resize-tl":
          return {
            x: originalX + dx,
            y: originalY + dy,
            width: Math.max(20, originalWidth - dx),
            height: Math.max(20, originalHeight - dy),
          };
        default:
          return {};
      }
    })();

    return updateMask(state, maskId, newProps);
  };

  // ========================================================================
  //  DOM MANIPULATION & SIDE EFFECTS (THE "EDGES")
  // ========================================================================

  /**
   * Creates the DOM element for a resize handle.
   * @param {DragType} type - The type of resize handle.
   * @returns {HTMLDivElement}
   */
  const createHandleElement = (type) => {
    const handle = document.createElement("div");
    handle.className = `yt-mask-handle ${type}`;
    handle.dataset.handleType = type;
    return handle;
  };

  /**
   * Renders the entire UI based on the current state. This function is the
   * primary source of side effects, manipulating the DOM to match the state.
   * @param {AppState} state - The state object to render.
   * @param {HTMLElement} container - The DOM element to render into.
   */
  const render = (state, container) => {
    // Synchronize mask elements with the state
    state.masks.forEach((maskData) => {
      let maskEl = container.querySelector(`#${maskData.id}`);
      if (!maskEl) {
        maskEl = document.createElement("div");
        maskEl.id = maskData.id;
        maskEl.className = "yt-mask";
        maskEl.dataset.maskId = maskData.id;
        maskEl.dataset.handleType = "move"; // The body of the mask is for moving
        // Add resize handles once on creation
        ["tl", "tr", "bl", "br"].forEach((pos) =>
          maskEl.appendChild(createHandleElement(`resize-${pos}`))
        );
        container.appendChild(maskEl);
      }

      // Apply styles from state
      Object.assign(maskEl.style, {
        transform: `translate(${maskData.x}px, ${maskData.y}px)`,
        width: `${maskData.width}px`,
        height: `${maskData.height}px`,
        zIndex: maskData.zIndex,
      });
    });

    // Garbage collect: remove any DOM elements that are no longer in the state
    Array.from(container.querySelectorAll(".yt-mask")).forEach((maskEl) => {
      if (!state.masks.some((m) => m.id === maskEl.id)) {
        maskEl.remove();
      }
    });
  };

  /**
   * Main function to initialize and run the script.
   * It finds the YouTube player, sets up the container, state, and event listeners.
   */
  const main = () => {
    const player = document.querySelector("#movie_player");
    if (!player) return; // Exit if player not found

    // --- Mutable State and Effectful Runner ---
    // This is the "impure" part of the application, which holds the current
    // state and connects event listeners to the pure state functions.
    let currentState = createInitialState();
    const appContainer = document.createElement("div");
    appContainer.id = "yt-mask-container";
    player.appendChild(appContainer);

    /**
     * A function that takes a new state, updates the central `currentState`
     * reference, and triggers a re-render. This is the bridge between
     * pure state transformations and the effectful world.
     * @param {AppState} newState
     */
    const updateStateAndRender = (newState) => {
      currentState = newState;
      render(currentState, appContainer);
    };

    // --- UI Control Elements (Side Effects) ---
    const addButton = document.createElement("button");
    addButton.id = "yt-mask-add-button";
    addButton.textContent = "Add Mask";
    appContainer.appendChild(addButton);
    addButton.addEventListener("click", () => {
      updateStateAndRender(addMask(currentState));
    });

    // --- Global Event Listeners (Side Effects) ---
    appContainer.addEventListener("mousedown", (e) => {
      const target = e.target;
      if (!target) return;

      const maskId = target.closest(".yt-mask")?.dataset.maskId;
      const handleType = target.dataset.handleType;

      if (!maskId || !handleType) return;
      e.preventDefault();
      e.stopPropagation();

      const mask = currentState.masks.find((m) => m.id === maskId);
      if (!mask) return;

      const dragInfo = {
        maskId: mask.id,
        type: handleType,
        startX: e.clientX,
        startY: e.clientY,
        originalX: mask.x,
        originalY: mask.y,
        originalWidth: mask.width,
        originalHeight: mask.height,
      };
      const withBroughtToFront = updateMask(currentState, maskId, {
        zIndex: 200,
      });
      updateStateAndRender(startDrag(withBroughtToFront, dragInfo));
    });

    document.addEventListener("mousemove", (e) => {
      if (!currentState.activeDrag) return;
      updateStateAndRender(handleDrag(currentState, e));
    });

    document.addEventListener("mouseup", () => {
      if (!currentState.activeDrag) return;
      // Lower z-index back to normal after finishing drag
      const finalState = endDrag(currentState);
      const maskId = currentState.activeDrag.maskId;
      const originalIndex = currentState.masks.findIndex(
        (m) => m.id === maskId
      );
      const finalStateWithZIndex = updateMask(finalState, maskId, {
        zIndex: 100 + originalIndex,
      });
      updateStateAndRender(finalStateWithZIndex);
    });

    // --- CSS Styles (Side Effect) ---
    const styles = `
            #yt-mask-container {
                position: absolute;
                top: 0; left: 0; width: 100%; height: 100%;
                pointer-events: none; /* Allows clicks to pass through to the video */
                overflow: hidden;
            }
            #yt-mask-add-button {
                position: absolute;
                top: 15px; left: 15px;
                z-index: 201; /* Above all masks */
                padding: 8px 12px;
                color: white;
                border: 1px solid rgba(255, 255, 255, 0.5);
                border-radius: 4px;
                cursor: pointer;
                pointer-events: auto; /* Button is clickable */
                font-family: "YouTube Noto", "Roboto", "Arial", sans-serif;
                font-size: 14px;
                background-color: rgba(15, 15, 15, 0.8);
                opacity: 0;
            }
            
            #yt-mask-add-button:hover { 
                background-color: rgba(50, 50, 50, 0.9); 
                opacity: 1;
            }

            .yt-mask {
                position: absolute;
                background-color: rgba(0, 0, 0, 1);
                border: 1px dashed rgba(255, 255, 255, 0.5);
                box-sizing: border-box;
                pointer-events: auto; /* The mask itself is interactive */
                cursor: move;
                will-change: transform, width, height;
            }
            .yt-mask-handle {
                position: absolute;
                width: 12px; height: 12px;
                background-color: rgba(255, 255, 255, 0.7);
                border: 1px solid black;
                border-radius: 2px;
                box-sizing: border-box;
            }
            .yt-mask-handle.resize-tl { top: -6px; left: -6px; cursor: nwse-resize; }
            .yt-mask-handle.resize-tr { top: -6px; right: -6px; cursor: nesw-resize; }
            .yt-mask-handle.resize-bl { bottom: -6px; left: -6px; cursor: nesw-resize; }
            .yt-mask-handle.resize-br { bottom: -6px; right: -6px; cursor: nwse-resize; }
        `;
    const styleSheet = document.createElement("style");
    styleSheet.innerText = styles;
    document.head.appendChild(styleSheet);

    // Initial render
    render(currentState, appContainer);
  };

  // Use a MutationObserver to robustly wait for the YouTube player to be ready,
  // which is necessary for modern single-page applications.
  const observer = new MutationObserver((mutations, obs) => {
    if (document.querySelector("#movie_player #yt-mask-container")) {
      return; // Already initialized
    }
    if (document.querySelector("#movie_player")) {
      main();
      // obs.disconnect(); // Can disconnect if you only want it to run once per page load.
      // Keeping it active can help if YouTube's layout drastically changes.
    }
  });

  observer.observe(document.body, {
    childList: true,
    subtree: true,
  });
})();
