module Web.View.Index.Show where
import Web.View.Prelude

data ShowView = ShowView {}

instance View ShowView where
    html ShowView { } = [hsx|
        {breadcrumb}
        <h1>Welcome to the Signer/Verifier App</h1>
        <br/>
        <h3>Sign File for Today &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href={pathTo NewMessageAction} class="btn btn-primary ml-4">+ Sign</a></h3>
        <br>
        <h3>Verify a File Signature<a href={pathTo NewVerificationMessageAction} class="btn btn-primary ml-4">+ Verify</a></h3>
    |]
        where
            breadcrumb = renderBreadcrumb
                            [ 
                                breadcrumbText "Main Menu"
                            ]