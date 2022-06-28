module Web.View.PubKeys.Index where
import Web.View.Prelude

data IndexView = IndexView { pubKeys :: [PubKey]  }

instance View IndexView where
    html IndexView { .. } = [hsx|
        {breadcrumb}

        <h1>Index<a href={pathTo NewPubKeyAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>PubKey</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach pubKeys renderPubKey}</tbody>
            </table>
            
        </div>
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "PubKeys" PubKeysAction
                ]

renderPubKey :: PubKey -> Html
renderPubKey pubKey = [hsx|
    <tr>
        <td>{pubKey}</td>
        <td><a href={ShowPubKeyAction (get #id pubKey)}>Show</a></td>
        <td><a href={EditPubKeyAction (get #id pubKey)} class="text-muted">Edit</a></td>
        <td><a href={DeletePubKeyAction (get #id pubKey)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]