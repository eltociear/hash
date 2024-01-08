import { Data, Effect, Exit } from "effect";

const DROP_MSB = 0b0111_1111;
const MSB = 0b1000_0000;
const MAX_SAFE_SHIFT = 4 * 7;

export class UnexpectedEndOfStreamError extends Data.TaggedError(
  "UnexpectedEndOfStream",
) {}

export class VariableIntegerOverflowError extends Data.TaggedError(
  "VariableIntegerOverflow",
) {}

class Checkpoint {
  protected offset: number;

  constructor(offset: number) {
    this.offset = offset;
  }

  public getOffset(): number {
    return this.offset;
  }
}

export class Reader {
  private offset: number;

  constructor(private slice: Uint8Array) {
    this.offset = 0;
  }

  public readByte() {
    if (this.offset >= this.slice.length) {
      return Effect.fail(new UnexpectedEndOfStreamError());
    }

    return Effect.succeed(this.slice[this.offset++]!);
  }

  public peakByte() {
    return Effect.fromNullable(this.slice[this.offset]);
  }

  public readBytes(length: number) {
    if (this.offset + length > this.slice.length) {
      return Effect.fail(new UnexpectedEndOfStreamError());
    }

    const bytes = this.slice.slice(this.offset, this.offset + length);
    this.offset += length;
    return Effect.succeed(bytes);
  }

  public readString(length: number) {
    const bytes = this.readBytes(length);

    return Effect.map(bytes, (bytes) => new TextDecoder().decode(bytes));
  }

  public readUInt8() {
    return Effect.map(this.readBytes(1), (bytes) =>
      new DataView(bytes.buffer).getUint8(0),
    );
  }

  public readInt8() {
    return Effect.map(this.readBytes(1), (bytes) =>
      new DataView(bytes.buffer).getInt8(0),
    );
  }

  public readUInt32() {
    return Effect.map(this.readBytes(4), (bytes) =>
      new DataView(bytes.buffer).getUint32(0, true),
    );
  }

  public readInt32() {
    return Effect.map(this.readBytes(4), (bytes) =>
      new DataView(bytes.buffer).getInt32(0, true),
    );
  }

  public readVarUInt32() {
    return Effect.gen(this, function* (_) {
      const checkpoint = this.saveCheckpoint();

      let value = 0;
      let shift = 0;

      _(
        Effect.addFinalizer((exit) => {
          if (Exit.isFailure(exit)) {
            this.restoreCheckpoint(checkpoint);
          }

          return Effect.unit;
        }),
      );

      while (true) {
        const byte = yield* _(this.readByte());

        let byteValue = byte & DROP_MSB;
        value |= byteValue << shift;
        shift += 7;

        if (shift > MAX_SAFE_SHIFT) {
          yield* _(new VariableIntegerOverflowError());
        }

        if ((byte & MSB) === 0) {
          break;
        }
      }

      return value;
    });
  }

  public saveCheckpoint(): Checkpoint {
    return new Checkpoint(this.offset);
  }

  public restoreCheckpoint(checkpoint: Checkpoint): void {
    this.offset = checkpoint.getOffset();
  }
}
