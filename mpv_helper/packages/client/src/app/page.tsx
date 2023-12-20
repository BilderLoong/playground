import { startMpv } from "./actions";
import { Main } from "./main";

export default function Home() {
  let stopMpv: (() => void) | null = null;
  return (
    <Main
      startMpv={async (...args) => {
        "use server";
        stopMpv = await startMpv(...args);
        return stopMpv;
      }}
      stopMpv={async () => {
        "use server";
        stopMpv?.();
      }}
    />
  );
}
