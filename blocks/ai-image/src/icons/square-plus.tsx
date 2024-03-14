import type { SvgIconProps } from "@mui/material";
import { SvgIcon } from "@mui/material";
import type { FunctionComponent } from "react";

export const SquarePlusIcon: FunctionComponent<SvgIconProps> = (props) => {
  return (
    <SvgIcon
      width="448"
      height="512"
      viewBox="0 0 448 512"
      fill="none"
      {...props}
    >
      <path d="M200 344V280H136C122.7 280 112 269.3 112 256C112 242.7 122.7 232 136 232H200V168C200 154.7 210.7 144 224 144C237.3 144 248 154.7 248 168V232H312C325.3 232 336 242.7 336 256C336 269.3 325.3 280 312 280H248V344C248 357.3 237.3 368 224 368C210.7 368 200 357.3 200 344zM0 96C0 60.65 28.65 32 64 32H384C419.3 32 448 60.65 448 96V416C448 451.3 419.3 480 384 480H64C28.65 480 0 451.3 0 416V96zM48 96V416C48 424.8 55.16 432 64 432H384C392.8 432 400 424.8 400 416V96C400 87.16 392.8 80 384 80H64C55.16 80 48 87.16 48 96z" />
    </SvgIcon>
  );
};
