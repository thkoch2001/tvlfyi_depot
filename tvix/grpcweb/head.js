const proto = {};

function loadProto(proto_, package_, merge_) {
  let protopart = proto_;
  for (let package_part of package_.split(/[.]/g)) {
    if (!protopart[package_part]) {
      protopart[package_part] = {};
    }
    protopart = protopart[package_part];
  }
  for (const k of Object.getOwnPropertyNames(merge_)) {
    protopart[k] = merge_[k];
  }
}

globalThis.proto = proto;
