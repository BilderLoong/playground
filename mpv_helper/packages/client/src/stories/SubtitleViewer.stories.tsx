import type { Meta, StoryObj } from "@storybook/react";
import FileUploader from "./FileUploader";
import {
  fireEvent,
  userEvent,
  within,
  screen,
  prettyDOM,
} from "@storybook/testing-library";

const meta: Meta<typeof FileUploader> = {
  component: FileUploader,
  parameters: {
    // More on how to position stories at: https://storybook.js.org/docs/7.0/react/configure/story-layout
    layout: "fullscreen",
  },
};

function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

export default meta;
type Story = StoryObj<typeof FileUploader>;

export const Default = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement);
    const fileUploader = canvas.getByTestId("file-uploader");
    screen.debug();
    screen.logTestingPlaygroundURL();
    await userEvent.click(fileUploader);
    await sleep(2000);
    await userEvent.keyboard("{enter}");
  },
} satisfies Story;
