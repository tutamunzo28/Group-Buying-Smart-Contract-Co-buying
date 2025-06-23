import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

Clarinet.test({
  name: "Can create a campaign with valid parameters",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;

    let block = chain.mineBlock([
      Tx.contractCall('group-buying', 'create-campaign', [
        types.ascii("Test Product"),
        types.uint(1000000),
        types.uint(5),
        types.uint(100000),
        types.uint(80000),
        types.uint(1000)
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    assertEquals(block.receipts[0].result.expectOk(), types.uint(1));
  },
});

Clarinet.test({
  name: "Cannot create campaign with invalid parameters",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;

    let block = chain.mineBlock([
      Tx.contractCall('group-buying', 'create-campaign', [
        types.ascii("Invalid Product"),
        types.uint(0),
        types.uint(5),
        types.uint(100000),
        types.uint(80000),
        types.uint(1000)
      ], deployer.address)
    ]);

    assertEquals(block.receipts.length, 1);
    block.receipts[0].result.expectErr(types.uint(110));
  },
});

Clarinet.test({
  name: "Can participate in active campaign",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user1 = accounts.get('wallet_1')!;

    let block = chain.mineBlock([
      Tx.contractCall('group-buying', 'create-campaign', [
        types.ascii("Test Product"),
        types.uint(1000000),
        types.uint(2),
        types.uint(100000),
        types.uint(80000),
        types.uint(1000)
      ], deployer.address)
    ]);

    let participateBlock = chain.mineBlock([
      Tx.contractCall('group-buying', 'participate', [
        types.uint(1),
        types.uint(5)
      ], user1.address)
    ]);

    assertEquals(participateBlock.receipts.length, 1);
    assertEquals(participateBlock.receipts[0].result.expectOk(), types.bool(true));
  },
});

Clarinet.test({
  name: "Cannot participate twice in same campaign",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user1 = accounts.get('wallet_1')!;

    chain.mineBlock([
      Tx.contractCall('group-buying', 'create-campaign', [
        types.ascii("Test Product"),
        types.uint(1000000),
        types.uint(2),
        types.uint(100000),
        types.uint(80000),
        types.uint(1000)
      ], deployer.address)
    ]);

    chain.mineBlock([
      Tx.contractCall('group-buying', 'participate', [
        types.uint(1),
        types.uint(5)
      ], user1.address)
    ]);

    let secondParticipation = chain.mineBlock([
      Tx.contractCall('group-buying', 'participate', [
        types.uint(1),
        types.uint(3)
      ], user1.address)
    ]);

    assertEquals(secondParticipation.receipts.length, 1);
    secondParticipation.receipts[0].result.expectErr(types.uint(106));
  },
});

Clarinet.test({
  name: "Can finalize successful campaign",
  async fn(chain: Chain, accounts: Map<string, Account>) {
    const deployer = accounts.get('deployer')!;
    const user1 = accounts.get('wallet_1')!;
    const user2 = accounts.get('wallet_2')!;

    chain.mineBlock([
      Tx.contractCall('group-buying', 'create-campaign', [
        types.ascii("Test Product"),
        types.uint(500000),
        types.uint(2),
        types.uint(100000),
        types.uint(80000),
        types.uint(1000)
      ], deployer.address)
