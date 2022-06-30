module Web.View.VerificationMessages.Show where
import Web.View.Prelude

data ShowView = ShowView { result :: Bool }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show File Signature Verification</h1>

        <p>{result}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                [   breadcrumbLink "Index" IndexAction,
                    breadcrumbLink "Verify Message Signature" CreateVerificationMessageAction,
                    breadcrumbText "Show Verification"
                ]