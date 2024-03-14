import type { SvgIconProps } from "@mui/material";
import { SvgIcon } from "@mui/material";
import type { FunctionComponent } from "react";

export const FilesLightIcon: FunctionComponent<SvgIconProps> = (props) => (
  <SvgIcon {...props} viewBox="0 0 448 512" fill="none">
    <path d="M160 384c-17.7 0-32-14.3-32-32V64c0-17.7 14.3-32 32-32H304v80c0 17.7 14.3 32 32 32h80c0 .3 0 .5 0 .8V352c0 17.7-14.3 32-32 32H160zM336 57.5L390 112H336V57.5zM160 0C124.7 0 96 28.7 96 64V352c0 35.3 28.7 64 64 64H384c35.3 0 64-28.7 64-64V144.8c0-12.7-5-24.8-13.9-33.8l-96-96.8C329.1 5.1 316.8 0 304 0H160zM32 112c0-8.8-7.2-16-16-16s-16 7.2-16 16V384c0 70.7 57.3 128 128 128H336c8.8 0 16-7.2 16-16s-7.2-16-16-16H128c-53 0-96-43-96-96V112z" />
  </SvgIcon>
);
