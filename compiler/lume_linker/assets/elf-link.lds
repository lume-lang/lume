/**
 * Linker script for Lume applications on ELF Linux systems.
 */

SECTIONS {
  .lumec.smaps : {
    __lumec_smaps_start = .;
    *(.lumec.smaps)
    __lumec_smaps_end = .;
  }

  .lumec.aliases : {
    __lumec_aliases_start = .;
    *(.lumec.aliases)
    __lumec_aliases_end = .;
  }

  .lumec.types : {
    __lumec_types_start = .;
    *(.lumec.types)
    __lumec_types_end = .;
  }

  .lumec.fields : {
    __lumec_fields_start = .;
    *(.lumec.fields)
    __lumec_fields_end = .;
  }

  .lumec.methods : {
    __lumec_methods_start = .;
    *(.lumec.methods)
    __lumec_methods_end = .;
  }

  .lumec.params : {
    __lumec_params_start = .;
    *(.lumec.params)
    __lumec_params_end = .;
  }

  .lumec.tparams : {
    __lumec_tparams_start = .;
    *(.lumec.tparams)
    __lumec_tparams_end = .;
  }
}
