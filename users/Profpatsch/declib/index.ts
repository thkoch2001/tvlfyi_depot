import generator, { MegalodonInterface } from 'megalodon';
import { Account } from 'megalodon/lib/src/entities/account';
import * as masto from 'megalodon/lib/src/entities/notification';
import { Status } from 'megalodon/lib/src/entities/status';
import * as rxjs from 'rxjs';
import { Observable } from 'rxjs';
import { NodeEventHandler } from 'rxjs/internal/observable/fromEvent';
import * as sqlite from 'sqlite';
import sqlite3 from 'sqlite3';
import * as parse5 from 'parse5';
import { mergeMap } from 'rxjs/operators';

type Events =
  | { type: 'connect'; event: [] }
  | { type: 'update'; event: Status }
  | { type: 'notification'; event: Notification }
  | { type: 'delete'; event: number }
  | { type: 'error'; event: Error }
  | { type: 'heartbeat'; event: [] }
  | { type: 'close'; event: [] }
  | { type: 'parser-error'; event: Error };

type Notification = masto.Notification & {
  type: 'favourite' | 'reblog' | 'status' | 'mention' | 'poll' | 'update';
  status: NonNullable<masto.Notification['status']>;
  account: NonNullable<masto.Notification['account']>;
};

class Main {
  private client: MegalodonInterface;
  private socket: Observable<Events>;
  private state!: State;
  private config: {
    databaseFile?: string;
    baseServer: string;
  };

  private constructor() {
    this.config = {
      databaseFile: process.env['DECLIB_DATABASE_FILE'],
      baseServer: process.env['DECLIB_MASTODON_SERVER'] ?? 'mastodon.xyz',
    };
    const ACCESS_TOKEN = process.env['DECLIB_MASTODON_ACCESS_TOKEN'];

    if (!ACCESS_TOKEN) {
      console.error('Please set DECLIB_MASTODON_ACCESS_TOKEN');
      process.exit(1);
    }
    this.client = generator('mastodon', `https://${this.config.baseServer}`, ACCESS_TOKEN);
    const websocket = this.client.publicSocket();
    function mk<Name extends string, Type>(name: Name): Observable<{ type: Name; event: Type }> {
      const wrap =
        (h: NodeEventHandler) =>
        (event: Type): void => {
          h({ type: name, event });
        };
      return rxjs.fromEventPattern<{ type: Name; event: Type }>(
        hdl => websocket.on(name, wrap(hdl)),
        hdl => websocket.removeListener(name, wrap(hdl)),
      );
    }
    this.socket = rxjs.merge(
      mk<'connect', []>('connect'),
      mk<'update', Status>('update'),
      mk<'notification', Notification>('notification'),
      mk<'delete', number>('delete'),
      mk<'error', Error>('error'),
      mk<'heartbeat', []>('heartbeat'),
      mk<'close', []>('close'),
      mk<'parser-error', Error>('parser-error'),
    );
  }

  static async init(): Promise<Main> {
    const self = new Main();
    self.state = await State.init(self.config);
    return self;
  }

  public main() {
    // const res = await this.getAcc({ username: 'grindhold', server: 'chaos.social' });
    // const res = await this.getAcc({ username: 'Profpatsch', server: 'mastodon.xyz' });
    // const res = await this.getStatus('111862170899069698');
    this.socket
      .pipe(
        mergeMap(async event => {
          switch (event.type) {
            case 'update': {
              await this.state.addStatus(event.event);
              console.log(`${event.event.account.acct}: ${event.event.content}`);
              console.log(await this.state.databaseInternal.all(`SELECT * from status`));
              break;
            }
            case 'notification': {
              console.log(`NOTIFICATION (${event.event.type}):`);
              console.log(event.event);
              console.log(event.event.status.content);
              const content = parseContent(event.event.status.content);
              if (content) {
                switch (content.command) {
                  case 'addbook': {
                    if (content.content[0]) {
                      const book = {
                        $owner: event.event.account.acct,
                        $bookid: content.content[0],
                      };
                      console.log('adding book', book);
                      await this.state.addBook(book);
                      await this.client.postStatus(
                        `@${event.event.account.acct} I have inserted book "${book.$bookid}" for you.`,
                        {
                          in_reply_to_id: event.event.status.id,
                          visibility: 'direct',
                        },
                      );
                    }
                  }
                }
              }
              break;
            }
            default: {
              console.log(event);
            }
          }
        }),
      )
      .subscribe();
  }

  private async getStatus(id: string): Promise<Status | null> {
    return (await this.client.getStatus(id)).data;
  }

  private async getAcc(user: { username: string; server: string }): Promise<Account | null> {
    const fullAccount = `${user.username}@${user.server}`;
    const res = await this.client.searchAccount(fullAccount, {
      limit: 10,
    });
    const accs = res.data.filter(acc =>
      this.config.baseServer === user.server
        ? (acc.acct = user.username)
        : acc.acct === fullAccount,
    );
    return accs[0] ?? null;
  }
}

type Interaction = {
  originalStatus: { id: string };
  lastStatus: { id: string };
};

class State {
  db!: sqlite.Database;
  private constructor() {}

  static async init(config: { databaseFile?: string }): Promise<State> {
    const s = new State();
    s.db = await sqlite.open({
      filename: config.databaseFile ?? ':memory:',
      driver: sqlite3.Database,
    });
    await s.db.run('CREATE TABLE books (owner text, bookid text)');
    await s.db.run('CREATE TABLE status (id text primary key, content json)');
    return s;
  }

  async addBook(opts: { $owner: string; $bookid: string }) {
    return await this.db.run('INSERT INTO books (owner, bookid) VALUES ($owner, $bookid)', opts);
  }

  async addStatus($status: Status) {
    return await this.db.run(
      `
      INSERT INTO status (id, content) VALUES ($id, $status)
      ON CONFLICT (id) DO UPDATE SET id = $id, content = $status
      `,
      {
        $id: $status.id,
        $status: JSON.stringify($status),
      },
    );
  }

  get databaseInternal() {
    return this.db;
  }
}

/** Parse the message; take the plain text, first line is the command any any successive lines are content */
function parseContent(html: string): { command: string; content: string[] } | null {
  const plain = contentToPlainText(html).split('\n');
  if (plain[0]) {
    return { command: plain[0].replace(' ', '').trim(), content: plain.slice(1) };
  } else {
    return null;
  }
}

/** Convert the Html content to a plain text (best effort), keeping line breaks */
function contentToPlainText(html: string): string {
  const queue: parse5.DefaultTreeAdapterMap['childNode'][] = [];
  queue.push(...parse5.parseFragment(html).childNodes);
  let res = '';
  let endOfP = false;
  for (const el of queue) {
    switch (el.nodeName) {
      case '#text': {
        res += (el as parse5.DefaultTreeAdapterMap['textNode']).value;
        break;
      }
      case 'br': {
        res += '\n';
        break;
      }
      case 'p': {
        if (endOfP) {
          res += '\n';
          endOfP = false;
        }
        queue.push(...el.childNodes);
        endOfP = true;
        break;
      }
      case 'span': {
        break;
      }
      default: {
        console.warn('unknown element in message: ', el);
        break;
      }
    }
  }
  return res.trim();
}

Main.init().then(
  m => {
    m.main();
  },
  rej => {
    throw rej;
  },
);
