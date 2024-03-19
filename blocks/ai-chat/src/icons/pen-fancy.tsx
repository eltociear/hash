import type { SvgIconProps } from "@mui/material";
import { SvgIcon } from "@mui/material";
import type { FunctionComponent } from "react";

export const PenFancyIcon: FunctionComponent<SvgIconProps> = (props) => {
  return (
    <SvgIcon
      width="512"
      height="512"
      viewBox="0 0 512 512"
      fill="none"
      {...props}
    >
      <path d="M399.4 59.9c7.1-7.6 17-11.9 27.3-11.9C447.3 48 464 64.7 464 85.3c0 10.4-4.3 20.2-11.9 27.3l-195.5 182-39.2-39.2 182-195.5zM426.7 0C403 0 380.4 9.8 364.2 27.2L170.8 234.9 97.2 257.6c-22.8 7-40.6 24.9-47.6 47.6L1.9 460.1c-9.4 30.7 19.3 59.4 50 50l154.8-47.6c22.8-7 40.6-24.9 47.6-47.6l22.6-73.6L484.8 147.8C502.2 131.6 512 109 512 85.3C512 38.2 473.8 0 426.7 0zM228.8 334.8l-20.3 65.9c-2.3 7.6-8.3 13.5-15.9 15.9L85 449.7 134.7 400c.4 0 .9 0 1.3 0c13.3 0 24-10.7 24-24s-10.7-24-24-24s-24 10.7-24 24c0 .4 0 .9 0 1.3L62.3 427 95.5 319.3c2.3-7.6 8.3-13.5 15.9-15.9l65.9-20.3 51.6 51.6z" />{" "}
    </SvgIcon>
  );
};
