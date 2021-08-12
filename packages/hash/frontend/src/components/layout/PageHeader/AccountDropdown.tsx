import { VoidFunctionComponent } from "react";
import { tw } from "twind";
import { Menu } from "@headlessui/react";
import { useMutation } from "@apollo/client";

import IconAvatar from "../../Icons/IconAvatar/IconAvatar";
import IconDropdown from "../../Icons/IconDropdown/IconDropdown";
import { logout as logoutMutation } from "../../../graphql/queries/user.queries";
import { Mutation } from "../../../graphql/apiTypes.gen";

type AccountDropdownProps = {
  name?: string;
  avatar?: string;
  onLoggedOut?: () => void;
};

export const AccountDropdown: VoidFunctionComponent<AccountDropdownProps> = ({
  name,
  avatar,
  onLoggedOut,
}) => {
  const [logout] = useMutation<Mutation>(logoutMutation, {
    onCompleted: (data) => {
      console.log("logout: ", data.logout);
      if (onLoggedOut) onLoggedOut();
    },
  });

  return (
    <Menu as="div" className={tw`relative`}>
      <Menu.Button
        title={name}
        className="flex items-center relative z-10 m-auto py-1 px-4 focus:outline-none"
      >
        {avatar ? (
          <img
            src={avatar}
            className={tw`h-6 w-6 border border(solid gray-200) rounded-full mr-3`}
          />
        ) : (
          <IconAvatar
            className={tw`border border(solid gray-200) rounded-full mr-3`}
          />
        )}
        <span className={tw`mr-2 font-bold`}>Account</span>
        <IconDropdown />
      </Menu.Button>
      <Menu.Items
        className={tw`absolute left-0 top-0 z-0 w-full px-4 pt-10 pb-2 bg-white border-1 rounded-md flex flex-col items-end text-right`}
      >
        <Menu.Item>
          <button
            onClick={() => logout()}
            className={tw`text-sm font-light border(b-1 transparent hover:gray-200) `}
          >
            Sign Out
          </button>
        </Menu.Item>
      </Menu.Items>
    </Menu>
  );
};
