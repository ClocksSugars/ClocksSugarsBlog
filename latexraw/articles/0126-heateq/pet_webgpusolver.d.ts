/* tslint:disable */
/* eslint-disable */

export function get_export_to_num(): number;

export function get_total_energy_in_one(): Promise<number>;

export function give_current_height(): number;

export function give_current_width(): number;

export function init_from_csv_buffer(): Promise<void>;

export function is_receiver_ready(): boolean;

export function junk_current_state(): void;

export function parse_csv(csv_as_string: string): string;

export function render_a_frame(): void;

export function rinit_with_xy(width: number, height: number): Promise<void>;

export function run_a_compute_iter(): void;

export function run_web(): Promise<void>;

export function send_output_to_export(): void;

export function update_values(n_times: number, kappa: number, delta_t: number, minT: number, maxT: number): void;

export function writeStateAsCSV(): Promise<string>;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
  readonly memory: WebAssembly.Memory;
  readonly get_export_to_num: () => [number, number, number];
  readonly get_total_energy_in_one: () => any;
  readonly give_current_height: () => [number, number, number];
  readonly give_current_width: () => [number, number, number];
  readonly init_from_csv_buffer: () => any;
  readonly is_receiver_ready: () => [number, number, number];
  readonly junk_current_state: () => [number, number];
  readonly parse_csv: (a: number, b: number) => [number, number, number, number];
  readonly render_a_frame: () => [number, number];
  readonly rinit_with_xy: (a: number, b: number) => any;
  readonly run_a_compute_iter: () => [number, number];
  readonly send_output_to_export: () => [number, number];
  readonly update_values: (a: number, b: number, c: number, d: number, e: number) => [number, number];
  readonly writeStateAsCSV: () => any;
  readonly run_web: () => void;
  readonly wasm_bindgen__convert__closures_____invoke__hc87f78881b5ae606: (a: number, b: number, c: any) => void;
  readonly wasm_bindgen__closure__destroy__h9d0be7857a79fa4f: (a: number, b: number) => void;
  readonly wasm_bindgen__convert__closures_____invoke__he4af440472ad3381: (a: number, b: number, c: any) => void;
  readonly wasm_bindgen__closure__destroy__hffb93927d9d25222: (a: number, b: number) => void;
  readonly wasm_bindgen__convert__closures_____invoke__h10ea31c4b068aac5: (a: number, b: number, c: any, d: any) => void;
  readonly __wbindgen_malloc: (a: number, b: number) => number;
  readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
  readonly __wbindgen_exn_store: (a: number) => void;
  readonly __externref_table_alloc: () => number;
  readonly __wbindgen_externrefs: WebAssembly.Table;
  readonly __wbindgen_free: (a: number, b: number, c: number) => void;
  readonly __externref_table_dealloc: (a: number) => void;
  readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
* Instantiates the given `module`, which can either be bytes or
* a precompiled `WebAssembly.Module`.
*
* @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
*
* @returns {InitOutput}
*/
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
* If `module_or_path` is {RequestInfo} or {URL}, makes a request and
* for everything else, calls `WebAssembly.instantiate` directly.
*
* @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
*
* @returns {Promise<InitOutput>}
*/
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
