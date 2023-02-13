import { supabaseClient } from "@supabase/auth-helpers-nextjs";

// supabaseClient.from("bla")
//   .select("wurst")
//   .eq("", 1)
//   .single()

export const signInWithOtpImpl = (supabase) => (email) => () =>
  supabase.auth.signInWithOtp({ email });

export const getSessionImpl = (supabase) => () => supabase.auth.getSession();
export const onAuthStateChangeImpl = (supabase) => (handler) => () =>
  supabase.auth.onAuthStateChange((_event, session) => handler(session));

export const signOutImpl = (supabase) => () => supabase.auth.signOut();
export const fromImpl = (client) => (table) => client.from(table);
export const selectQueryImpl = (queryBuilder) => (projection) =>
  queryBuilder.select(projection);

export const selectQueryWithCountImpl =
  (queryBuilder) => (projection) => (count) =>
    queryBuilder.select(projection, { count });

export const deleteImpl = (queryBuilder) => queryBuilder.delete();

export const updateImpl = (queryBuilder) => (record) =>
  queryBuilder.update(record);

export const eqRunImpl = (builder) => (key, value) => () =>
  builder.eq(key, value);

export const eqImpl = (builder) => (key, value) => builder.eq(key, value);

export const singleImpl = (filterBuilder) => () => filterBuilder.single();

export const maybeSingleImpl = (filterBuilder) => () =>
  filterBuilder.maybeSingle();

export const rangeImpl = (from) => (to) => (filterBuilder) => () =>
  filterBuilder.range(from, to);
export const upsertImpl = (queryBuilder) => (values) =>
  queryBuilder.upsert(values);

export const selectRunImpl = (queryBuilder) => (input) => () =>
  queryBuilder.select(input);

export const invokeImpl = (client) => (functionName, body, headers) => () =>
  client.functions.invoke(functionName, {
    body,
    headers,
  });
