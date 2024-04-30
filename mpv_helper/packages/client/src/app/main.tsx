"use client";
import React, { useEffect, useRef } from "react";
import { Command, keyMessage } from "../protocols/ws";
import { startMpv, stopMpv } from "./actions";
import { useCommutateToWS } from "./hooks";

export const Main = () => {
  useCommutateToWS();
  return <div>Click some key.</div>;
};
