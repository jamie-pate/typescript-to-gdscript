interface AKindValue {
  a: true;
}

interface BKindValue {
  b: true;
}

interface KindEnvelope<T> {
  type: string;
  value: T;
}

interface AKind extends KindEnvelope<AKindValue> {
  type: 'a';
}

interface BKind extends KindEnvelope<BKindValue> {
  type: 'b';
}

/**
 * This union kind isn't easy to automatically convert.
 * In this case it's your job to create a class that discriminates between AKind and BKind
 * so you can just pass `AnyKind.new(anyKind)` and your gdscript code will be able to
 * check which kind it is...
 * @typescript-to-gdscript-gd-impl
 */
export type AnyKind = AKind | BKind;
