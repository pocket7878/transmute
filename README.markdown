# Transmute

Common Lisp Deatabase Independent Migration tool.

Also provides clack middleware to check pending-migration.

## Usage

```lisp
;;Run migrations.
(migrate (:mysql
          :database-name "test"
          :username "nobody"
          :password "1234"))

*migration-dir*
#=> #P"db/migrate"

;;Create migration under *migration-dir*.
(gen-migration "create-users")
```

## Installation

## API

### transmute package

- \*migration-dir\*
- migration [db-spec]
- gen-new-migration [migration-name]

### clack-migration-checker package

- <clack-migration-checker>
- <caveman-migration-checker>

## TODO

- Migration template.
- Warning

## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2015 Masato Sogame (poketo7878@gmail.com)

## License

Licensed under the LLGPL License.
