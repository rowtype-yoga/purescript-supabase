'use client'
import { useUser } from "@supabase/auth-helpers-react";
import { createBrowserSupabaseClient } from "@supabase/auth-helpers-nextjs";

export const useUserImpl = useUser;


export const createBrowserClient = () => createBrowserSupabaseClient();

export const createBrowserClientWithOptionsInternal = (options) => () =>
  createBrowserSupabaseClient(options);
