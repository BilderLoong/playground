import { startMpv } from "./actions";
import { Main } from "./main";

export default function Home() {
  let stopMpv: (() => void) | null = null;
  return <Main />;
}
