module Web.View.VerificationMessages.Show where
import Web.View.Prelude

data ShowView = ShowView { result :: Bool, date :: Day, fileName :: Text }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show File Signature Verification</h1>


        
        <p style=""><b>Result of verification: &nbsp; </b><u> {result} </u></p>
        <p><b> Date of signature: </b> {date} </p>
        <p><b> Name of file: </b> {fileName} </p>
    |]
        where
            breadcrumb = renderBreadcrumb
                [   breadcrumbLink "Index" IndexAction,
                    breadcrumbLink "Verify Message Signature" NewVerificationMessageAction,
                    breadcrumbText "Show Verification"
                ]