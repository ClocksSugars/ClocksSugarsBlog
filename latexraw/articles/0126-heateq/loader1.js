
import init from "./pet_webgpusolver.js";
import {
   run_a_compute_iter,
   render_a_frame,
   update_values,
   send_output_to_export,
   //setup_temp_receiver,
   is_receiver_ready,
   get_export_to_num,
   junk_current_state,
   rinit_with_xy,
   parse_csv,
   give_current_width,
   give_current_height,
   init_from_csv_buffer,
   get_total_energy_in_one,
   writeStateAsCSV
} from "./pet_webgpusolver.js";

function setup_loader1() {
   const edit_thing = document.getElementById("loader1_give_content");
   const canvas = document.createElement("canvas");
   canvas.id = "canvas";
   canvas.style = "background-color: black;max-width: 600px;"
   edit_thing.appendChild(canvas);
   const button = document.createElement("button");
   button.type = "button";
   button.id = "activate_sim_button";
   button.textContent = "Click here to Activate GPU app";
   button.classList.add("flex-col")
   edit_thing.appendChild(button);
}

setup_loader1();

var max_N = 52488;
var N_add = 100;
var current_N = 0;
var current_time = 0;
var current_delta_t = 1.9073486328125e-06;
var stop_compute = false;
var get_total_temp = true;

document.getElementById("activate_sim_button").addEventListener("click", async (event) => {
   document.getElementById("activate_sim_button").remove();
   var paren = document.getElementById("loader1_give_content");
   const resp = await fetch("./loader2.html");
   const html = await resp.text();
   paren.innerHTML += html;

   renderMathInElement(paren, katexargs)

   async function run() {
      await init();
   }

   await run();

   console.log("WASM Loaded");

   async function init_energy() {
      var init_energy_val = await get_total_energy_in_one();
      document.getElementById("total_energy_goes_here").textContent = init_energy_val;
   }
   init_energy();

   function validateintbox(element) {
      var val = Math.floor(element.value);
      if (val < 1) {val = 1}
      element.value = val;
   }

   document.getElementById("delta_x").addEventListener("change", (event) => {
      var val = Math.floor(
         1. / document.getElementById("delta_x").value
      );
      if (val < 1) {val = 1}
      document.getElementById("width_val").value = val;
   })

   document.getElementById("delta_y").addEventListener("change", (event) => {
      var val = Math.floor(
         1. / document.getElementById("delta_y").value
      );
      if (val < 1) {val = 1}
      document.getElementById("height_val").value = val;
   })

   document.getElementById("width_val").addEventListener("change", (event) => {
      var val = Math.floor(
         document.getElementById("width_val").value
      );
      if (val < 1) {val = 1}
      let length_val = document.getElementById("length_val");
      document.getElementById("delta_x").value = 1.0 / val;
   })

   document.getElementById("height_val").addEventListener("change", (event) => {
      var val = Math.floor(
         document.getElementById("height_val").value
      );
      if (val < 1) {val = 1}
      let length_val = document.getElementById("height_val");
      document.getElementById("delta_y").value = 1.0 / val;
   })

   document.getElementById("max_N").addEventListener("change", (event) => {
      validateintbox(event.target);
   })
   document.getElementById("N_add").addEventListener("change", (event) => {
      validateintbox(event.target);
   })

   document.getElementById("auto_delta_t").addEventListener("click", (event) => {
      var width = document.getElementById("width_val").value;
      var height = document.getElementById("height_val").value;
      var kappa = document.getElementById("kappa").value;
      var safety_factor = document.getElementById("safety_factor").value;
      document.getElementById("delta_t").value = safety_factor / (2 * kappa * (width ** 2 + height ** 2))
   })

   document.getElementById("auto_max_N").addEventListener("click", (event) => {
      var theval = Math.ceil(document.getElementById("max_time").value / parseFloat(document.getElementById("delta_t").value));
      document.getElementById("max_N").value = theval;
   })

   function run_compute() {
      run_a_compute_iter();
      if (get_total_temp) {
         send_output_to_export();
         render_a_frame();
         //setup_temp_receiver();
         get_total_temp = false;
      } else {
         render_a_frame();
      }

      current_N = current_N + N_add
      current_time = current_time + current_delta_t * N_add;

      let receiver_response = is_receiver_ready();
      if (receiver_response) {
         //console.log(`tried to export num with receiver ${receiver_response}`)
         let total_energy = get_export_to_num();
         document.getElementById("total_energy_goes_here").textContent = `${total_energy}`
         document.getElementById("total_time_goes_here").textContent = `${current_time}`
         get_total_temp = true;
      }

      if ((current_N < max_N) & (~stop_compute)) {requestAnimationFrame(run_compute);}
   }

   document.getElementById("compute").addEventListener("click", (event) => {
      current_N = 0;
      stop_compute = false;
      requestAnimationFrame(run_compute);
   })

   document.getElementById("break").addEventListener("click", (event) => {
      stop_compute = true;
   })

   document.getElementById("update_vals").addEventListener("click", (event) => {
      max_N = parseInt(document.getElementById("max_N").value);
      N_add = parseInt(document.getElementById("N_add").value);
      current_delta_t = parseFloat(document.getElementById("delta_t").value);
      update_values(
         document.getElementById("N_add").value,
         document.getElementById("kappa").value,
         document.getElementById("delta_t").value,
         document.getElementById("min_T").value,
         document.getElementById("max_T").value,
      )
   })

   async function reset_state_with_dims() {
      stop_compute = true;
      junk_current_state();
      var width = document.getElementById("width_val").value;
      var height = document.getElementById("height_val").value;
      await rinit_with_xy(width, height);
      current_N = 0;
      max_N = document.getElementById("max_N").value;
      update_values(
         document.getElementById("N_add").value,
         document.getElementById("kappa").value,
         document.getElementById("delta_t").value,
         document.getElementById("min_T").value,
         document.getElementById("max_T").value,
      );
      get_total_temp = true;
      current_grid_shape.textContent = `${width}x${height}`;
   }

   function showMessage(thestring) {
      document.getElementById("message_receiver").textContent = thestring;
   }

   document.getElementById("send_xy").addEventListener("click", (event) => {
      reset_state_with_dims();
      current_time = 0;
   })

   async function do_csv_process() {
      var file = document.getElementById("take_in_csv").files[0];
      if (!file) {
         showMessage("no file!")
         return;
      }
      // if (!file.type.startsWith("csv")) {
      //     showMessage("Unsupported file type. Please select a CSV file.", "error");
      //     return;
      //   }
      const reader = new FileReader();
      var result = "not done yet";
      reader.onload = async () => {
         stop_compute = true;
         result = parse_csv(reader.result);
         showMessage(result);
         if (result.startsWith("success!")) {
            junk_current_state();
            await init_from_csv_buffer();
            document.getElementById("total_energy_goes_here").textContent = await (get_total_energy_in_one());
            var width = give_current_width();
            var height = give_current_height();
            document.getElementById("width_val").value = width;
            document.getElementById("height_val").value = height;
            document.getElementById("delta_x").value = 1.0 / width;
            document.getElementById("delta_y").value = 1.0 / height;
         } else {
            showMessage(result);
         };
      };
      reader.readAsText(file);
   }

   document.getElementById("send_csv_to_gpu").addEventListener("click", (event) => {
      do_csv_process();
   })

   document.getElementById("over_error_threshhold").addEventListener("click", (event) => {
      var width = document.getElementById("width_val").value;
      var height = document.getElementById("height_val").value;
      var kappa = document.getElementById("kappa").value;
      document.getElementById("safety_factor").value = 1.001;
      var newdeltat = 1.001 / (2 * kappa * (width ** 2 + height ** 2));
      document.getElementById("delta_t").value = newdeltat;
      document.getElementById("N_add").value = 10;
      update_values(
         document.getElementById("N_add").value,
         document.getElementById("kappa").value,
         newdeltat,
         document.getElementById("min_T").value,
         document.getElementById("max_T").value,
      )
   })

   document.getElementById("export_csv").addEventListener("click", async (event) => {
      const the_data = await writeStateAsCSV();
      var link = document.createElement('a');
      link.download = 'heateq_state.csv';
      var blob = new Blob([the_data], { type: 'text/plain' });
      link.href = window.URL.createObjectURL(blob);
      link.click();
   })
})
