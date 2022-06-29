module Web.View.VerificationMessages.Show where
import Web.View.Prelude

data ShowView = ShowView { result :: Bool }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show VerificationMessage</h1>

        <p>{result}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ 
                                breadcrumbText "Show VerificationMessage"
                            ]