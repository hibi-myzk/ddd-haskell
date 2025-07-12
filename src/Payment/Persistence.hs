module Payment.Persistence where

import Payment.Undefined (DotDotDot, dotDotDot, notImplemented)

type UpdatedInvoice = DotDotDot    
let applyPayment _ = dotDotDot()
type UnpaidInvoice = DotDotDot
let isFullyPaid _ :bool = dotDotDot() 

data InvoicePaymentResult = FullyPaid | PartiallyPaid DotDotDot
  deriving (Show, Eq)

-- domain workflow: pure function
applyPaymentToInvoice 
    :: UnpaidInvoice 
    -> DotDotDot  -- payment
    -> InvoicePaymentResult
applyPaymentToInvoice unpaidInvoice payment =
    -- apply payment
    updatedInvoice = applyPayment payment unpaidInvoice

    -- handle different outcomes
    if isFullyPaid updatedInvoice
      then FullyPaid 
      else PartiallyPaid updatedInvoice 
    -- return PartiallyPaid or FullyPaid

let loadInvoiceFromDatabase _ = notImplemented()
let markAsFullyPaidInDb _ = notImplemented()
let markAsPartiallyPaidInDb _ = notImplemented()
let postInvoicePaidEvent _ = notImplemented()
let updateInvoiceInDb _ = notImplemented()

data PayInvoiceCommand = PayInvoiceCommand
  { invoiceId :: DotDotDot,
    payment :: DotDotDot
  }

payInvoice :: 
  PayInvoiceCommand 
  -> (UnpaidInvoice -> DotDotDot) -- load from DB
  -> (DotDotDot -> DotDotDot)      -- mark as fully paid in DB
  -> (UpdatedInvoice -> DotDotDot) -- update invoice in DB
  -> InvoicePaymentResult           -- return type
payInvoice 
  loadUnpaidInvoiceFromDatabase -- dependency
  markAsFullyPaidInDb           -- dependency
  updateInvoiceInDb             -- dependency
  payInvoiceCommand =

  -- load from DB
  let invoiceId = payInvoiceCommand.InvoiceId
  let unpaidInvoice = 
      loadUnpaidInvoiceFromDatabase invoiceId 

  -- call into pure domain
  let payment = 
      payInvoiceCommand.Payment
  let paymentResult = 
      applyPayment unpaidInvoice payment

  -- handle result
  case paymentResult of
    FullyPaid ->
      markAsFullyPaidInDb(invoiceId)
      postInvoicePaidEvent(invoiceId)
    PartiallyPaid updatedInvoice ->
      updateInvoiceInDb updatedInvoice   
