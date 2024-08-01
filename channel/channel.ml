type 'a channel =
  { queue : 'a Core.Queue.t
  ; mutex : Mutex.t
  ; cond : Condition.t
  }

let create () =
  { queue = Core.Queue.create (); mutex = Mutex.create (); cond = Condition.create () }
;;

let send chan msg =
  Mutex.lock chan.mutex;
  Core.Queue.enqueue chan.queue msg;
  Condition.signal chan.cond;
  Mutex.unlock chan.mutex
;;

let receive chan =
  Mutex.lock chan.mutex;
  while Core.Queue.is_empty chan.queue do
    Condition.wait chan.cond chan.mutex
  done;
  let msg = Core.Queue.dequeue chan.queue in
  Mutex.unlock chan.mutex;
  msg
;;

let try_receive chan =
  Mutex.lock chan.mutex;
  let msg = Core.Queue.dequeue chan.queue in
  Mutex.unlock chan.mutex;
  msg
;;
