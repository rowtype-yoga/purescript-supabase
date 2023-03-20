import { useUser } from "@supabase/auth-helpers-react";

export const useUserImpl = useUser;

import { createBrowserSupabaseClient } from "@supabase/auth-helpers-nextjs";

export const createBrowserClient = () => createBrowserSupabaseClient();

export const createBrowserClientWithOptionsInternal = (options) => () =>
  createBrowserSupabaseClient(options);
