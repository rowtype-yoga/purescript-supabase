import { supabaseClient } from "@supabase/auth-helpers-nextjs";

export const storage = (client) => client.storage;

export const fromImpl = (storage) => (table) => storage.from(table);

export const uploadImpl = (queryBuilder) => (filePath, file, fileOptions) => () =>
  queryBuilder.upload(filePath, file, fileOptions);

export const downloadImpl = (queryBuilder) => (filePath) => () =>
  queryBuilder.download(filePath);


export const removeImpl = (queryBuilder) => (filePaths) => () =>
  queryBuilder.remove(filePaths);
