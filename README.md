# user-keymap

This Emacs package allows users to maintain keybindings in the
customisation variable `user-keymap-list`.  The keybindings are
activated in the globalised minor mode `user-keymap-mode`.

You can use `customize-variable` with `user-keymap-list` to edit your
key bindings. Then they will be presented in a conveniently readable
and editable way. `user-keymap-list` is an assoc list of key sequences
and commands (represented as there symbols). Customisations of this
list will immediately become active if `global-user-keymap-mode` is
enabled.

So you can add this line to your `.emacs` initialisation file to
automatically activate your bindings every time you start emacs:
```
(global-user-keymap-mode 1)
```

After some time `user-keymap-list` might grow and become unorganised.
Therefore, two auxiliary commands are provided to sort the list on
commands resp. key sequences:

- `user-keymap-sort-on-commands`
- `user-keymap-sort-on-keysequences`

You may find it more convenient to add and delete entries using these
commands:

- `user-keymap-add`

   prompts for a key sequence and warns you in case the key sequence is
   already bound to a function. You can then decide to select another
   key sequence.  Next the command prompts for a command, the default
   being the symbol at point.  If not aborted during the prompts a
   pair is built from user input and added to `user-keymap-list`. The
   new setting is activated and saved to the initialisation file.

- `user-keymap-delete`

   prompts for a command with the default set to the symbol at point.
   All entries for this command are deleted from `user-keymap-list`.
   The new setting is activated and saved to the initialisation file.
