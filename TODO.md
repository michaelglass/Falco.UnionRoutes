# Future Ideas

## Body<'T> Support

Add `Body<'T>` marker type for JSON request body extraction, following the `Pre<'T>` pattern:

```fsharp
type PostRoute =
    | Create of Pre<UserId> * Body<CreatePostDto>                // required body
    | Patch of Pre<UserId> * id: PostId * Body<PatchDto> option  // optional body
```

- No field name needed (like `Pre<'T>`, unlike `Query<'T>`)
- `Body<'T>` - error if missing/malformed
- `Body<'T> option` - `None` if empty, `Some` if present
- Uses Falco's `Request.getJson<'T>` or `Request.mapJson`

## Other Ideas

- **API Client Generation**: Generate F#/TypeScript clients from route types
- **OpenAPI Spec Generation**: Generate OpenAPI specs at compile-time
- **Response Type Safety**: `[<Returns<PostDto>>]` attribute for type-checked responses
- **Handler Scaffolding**: Generate handler stubs with correct signatures
