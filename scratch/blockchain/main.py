from flask import Flask, jsonify, request
from hashlib import sha256
from datetime import datetime
from urllib.parse import urlparse

import json
import requests
import uuid

################################################################################
# Helper Functions
################################################################################

def hash(x):
  return sha256(x).hexdigest()

def is_pow_valid(guess, prev_proof):
  """
  Return true if the hash of `guess` + `prev_proof` has 4x leading zeros.
  """
  return hash(str(guess + prev_proof).encode("utf8"))[:4] == "0000"

################################################################################
# Classes
################################################################################

class Node(object):
  def __init__(self, host="0.0.0.0", port=8000):
    self.app = Flask(__name__)
    self.define_api()
    self.identifier = str(uuid.uuid4())
    self.blockchain = Blockchain()
    self.neighbors = set()

  def add_neighbors(self, urls=None):
    for url in urls:
      parsed = urlparse(url)
      if not parsed.netloc:
        raise ValueError("Must pass valid URLs for neighbors")
      self.neighbors.add(parsed.netloc)

  def decode_chain(chain_json):
    return Blockchain(
        blocks=[
            Block(
                index=block["index"],
                ts=block["ts"],
                transactions=[
                    Transaction(
                        origin=tx["origin"],
                        target=tx["target"],
                        amount=tx["amount"])
                        for tx in block["ts"]
                ],
                proof=block["proof"],
                prev_hash=block["prev_hash"])
                for block in chain_json["blocks"]
        ],
        transactions=[
            Transaction(
                origin=tx["origin"],
                target=tx["target"],
                amount=tx["amount"])
                for tx in chain_json["transactions"]
        ])

  def resolve_conflicts(self):
    auth_chain, auth_length = self.blockchain, len(self.blockchain)

    for neighbor in self.neighbors:
      res = requests.get(f"http://{neighbor}/chain")
      if res.status_code == 200 and res.json()["length"] > auth_length:
         decoded_chain = decode_chain(res.json()["chain"])
         if Blockchain.is_valid(decoded_chain):
           auth_length = res.json()["length"]
           auth_chain = decoded_chain

      self.blockchain = auth_chain

  def define_api(self):
    def msg(x):
      return jsonify({"message": x})

    ############################################################################
    # /
    ############################################################################

    @self.app.route("/healthz", methods={"GET"})
    def healthz():
      return "ok"

    @self.app.route("/reset", methods={"GET"})
    def reset():
      self.blockchain = Blockchain()
      return msg("Success")

    @self.app.route("/mine", methods={"GET"})
    def mine():
      # calculate POW
      proof = self.blockchain.prove_work()

      # reward miner
      self.blockchain.add_transaction(
          origin="0", # zero signifies that this is a newly minted coin
          target=self.identifier,
          amount=1)

      # publish new block
      self.blockchain.add_block(proof=proof)
      return msg("Success")

    ############################################################################
    # /transactions
    ############################################################################

    @self.app.route("/transactions/new", methods={"POST"})
    def new_transaction():
      payload = request.get_json()

      self.blockchain.add_transaction(
          origin=payload["origin"],
          target=payload["target"],
          amount=payload["amount"])
      return msg("Success")

    ############################################################################
    # /blocks
    ############################################################################

    @self.app.route("/chain", methods={"GET"})
    def view_blocks():
      return jsonify({
          "length": len(self.blockchain),
          "chain": self.blockchain.dictify(),
      })

    ############################################################################
    # /nodes
    ############################################################################
    @self.app.route("/node/neighbors", methods={"GET"})
    def view_neighbors():
      return jsonify({"neighbors": list(self.neighbors)})

    @self.app.route("/node/register", methods={"POST"})
    def register_nodes():
      payload = request.get_json()["neighbors"]
      payload = set(payload) if payload else set()
      self.add_neighbors(payload)
      return msg("Success")

    @self.app.route("/node/resolve", methods={"GET"})
    def resolve_nodes():
      self.resolve_conflicts()
      return msg("Success")

  def run(self):
    self.app.run(host="0.0.0.0", port=8000)


class Blockchain(object):
  def __init__(self, blocks=None, transactions=None):
    self.blocks = blocks or []
    self.transactions = transactions or []
    self.add_block()

  def __len__(self):
    return len(self.blocks)

  def __iter__(self):
    for block in self.blocks:
      yield block

  def prove_work(self):
    guess, prev_proof = 0, self.blocks[-1].proof or 0
    while not is_pow_valid(guess, prev_proof):
      guess += 1
    return guess

  def add_block(self, prev_hash=None, proof=None):
    b = Block(
        index=len(self),
        transactions=self.transactions,
        prev_hash=self.blocks[-1].hash() if self.blocks else None,
        proof=proof)
    self.blocks.append(b)
    return b

  def adopt_blocks(self, json_blocks):
    pass

  def add_transaction(self, origin=None, target=None, amount=None):
    tx = Transaction(origin=origin, target=target, amount=amount)
    self.transactions.append(tx)

  @staticmethod
  def is_valid(chain):
    prev_block = next(chain)

    for block in chain:
      if block.prev_hash != prev_block.hash() or not is_pow_valid(prev_block.proof, block.proof):
        return False
      prev_block = block

    return True

  def dictify(self):
    return {
        "blocks": [block.dictify() for block in self.blocks],
        "transactions": [tx.dictify() for tx in self.transactions],
    }


class Block(object):
  def __init__(self, index=None, ts=None, transactions=None, proof=None, prev_hash=None):
    self.index = index
    self.ts = ts or str(datetime.now())
    self.transactions = transactions
    self.proof = proof
    self.prev_hash = prev_hash

  def hash(self):
    return sha256(self.jsonify().encode()).hexdigest()

  def dictify(self):
    return {
        "index": self.index,
        "ts": self.ts,
        "transactions": [tx.dictify() for tx in self.transactions],
        "proof": self.proof,
        "prev_hash": self.prev_hash,
    }

  def jsonify(self):
    return json.dumps(self.dictify(), sort_keys=True)

class Transaction(object):
  def __init__(self, origin=None, target=None, amount=None):
    if None in {origin, target, amount}:
      raise ValueError("To create a Transaction, you must provide origin, target, and amount")

    self.origin = origin
    self.target = target
    self.amount = amount

  def dictify(self):
    return {
        "origin": self.origin,
        "target": self.target,
        "amount": self.amount,
    }

  def jsonify(self):
    return json.dumps(self.dictify(), sort_keys=True)

################################################################################
# Main
################################################################################

def run():
  Node(host="0.0.0.0", port=8000).run()

if __name__ == "__main__":
  run()
