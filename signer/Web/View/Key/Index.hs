module Web.View.Key.Index where
import Web.View.Prelude

data IndexView = IndexView { key :: [Key]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewKeyAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Key</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach key renderKey}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Keys" KeysAction
                ]

renderKey :: Key -> Html
renderKey key = [hsx|
    <tr>
        <td>{key}</td>
        <td><a href={ShowKeyAction (get #id key)}>Show</a></td>
        <td><a href={EditKeyAction (get #id key)} class="text-muted">Edit</a></td>
        <td><a href={DeleteKeyAction (get #id key)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]