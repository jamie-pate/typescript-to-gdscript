// test to make sure we don't loop infinitely on import cycles.
import { TestInterfaceRoot } from "./test-interface";

export interface ImportedInterface {
    value: string,
}

export type ImportedRecord1 = Record<string, string>;

export type ImportedArray1 = string[];

export type ImportedRecord2 = Record<string, ImportedInterface>;

export type ImportedArray2 = ImportedInterface[];

export class Ignored {}

export const SOME_VAR = 'ignored';
