  /* Add module environment functions newly added in Emacs 32 here.
     Before Emacs 32 is released, remove this comment and start
     module-env-33.h on master (see admin/release-branch.txt).  */

  /* Get pointer to the pixel buffer of CANVAS.  The buffer is in row
     major order and has the size width * height.  The pixel format is
     ARGB32.  The pointer will be valid as long as CANVAS is alive.
     Return NULL in case error.   */
/* TODO: Document that buffer is only valid as long as CANVAS is alive and size does not change. */
  uint32_t* (*canvas_data) (emacs_env *env, emacs_value canvas)
    EMACS_ATTRIBUTE_NONNULL(1);
