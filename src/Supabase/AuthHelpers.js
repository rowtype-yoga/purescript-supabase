export {
  UserProvider as userProvider,
  useUser as useUserImpl,
} from "@supabase/auth-helpers-react";

import { createBrowserSupabaseClient } from "@supabase/auth-helpers-nextjs";

export const createBrowserClient = () => createBrowserSupabaseClient();
