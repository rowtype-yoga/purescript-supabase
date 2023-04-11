export const sendImpl = (what, channel) => channel.send(what)

export const onImpl = (listenType, filter, callback, channel) => channel.on(listenType, filter, callback)

export const subscribeImpl = (callback,timeout, channel) => channel.subscribe(callback,timeout)
